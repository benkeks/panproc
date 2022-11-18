package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics

object Semantics:
  type Env = Map[String, Syntax.ProcessExpression]
  type ProcLabel = Unit

class Semantics extends AbstractOperationalSemantics[Syntax.ProcessExpression, Semantics.Env, Syntax.ProcessExpression, Syntax.Label, Semantics.ProcLabel]:

  import Semantics._

  override def stateIds(ex: Syntax.ProcessExpression) = ex

  override def globalEnvironment(ccsDef: Syntax.ProcessExpression): (Env, Iterable[Syntax.ProcessExpression]) =
    val procDefs = ccsDef.asInstanceOf[Syntax.Definition].defs.collect {
      case d@Syntax.ProcessDeclaration(name, proc) =>
        (name, proc)
    }.toMap
    (procDefs, procDefs.values)

  override def localSemantics(procEnv: Env)(e: Syntax.ProcessExpression)
    : List[(Syntax.Label, Syntax.ProcessExpression)] =
    e match
      case Syntax.Prefix(l, proc) =>
        List((l, proc))
      case Syntax.Choice(procs) =>
        procs.flatMap(localSemantics(procEnv)(_))
      case Syntax.Parallel(procs) =>
        List()
        // val initialSteps = procs.map(localSemantics(procEnv)(_))
        // val initialStepsGrouped =
        //   initialSteps.map(_.groupBy { case (a, _) => (toInput(a), isOutput(a)) }).zipWithIndex
        // val newSyncSteps = for {
        //   (initsA, iA) <- initialStepsGrouped
        //   (initsB, iB) <- initialStepsGrouped.filterNot(_._2 == iA)
        //   bOutputs = initsB.filterKeys(_._2)
        //   ((actionA, _), succA) <- initsA.filterKeys(k => !k._2).toList
        //   pA <- succA
        //   pB <- bOutputs.getOrElse((actionA, true), Nil)
        // } yield {
        //   val newProcs = procs.zipWithIndex.map { case (p, i) =>
        //     if (i == iA) {
        //       pA._2
        //     } else if (i == iB) {
        //       pB._2
        //     } else {
        //       p
        //     }
        //   }
        //   (silentAction, Syntax.Parallel(newProcs))
        // }
        // val newInterleavedSteps: List[(A, Syntax.ProcessExpression)] = for {
        //   (initsA, iA) <- initialSteps.zipWithIndex
        //   (a, p) <- initsA
        // } yield {
        //   val newProcs = procs.updated(iA, p)
        //   (a, Syntax.Parallel(newProcs))
        // }
        // newSyncSteps ++ newInterleavedSteps
      case Syntax.Restrict(names, proc) =>
        val aNames = names.map(_.name)
        for {
          (a, p) <- localSemantics(procEnv)(proc)
          if !aNames.contains(a)
        } yield {
          (a, Syntax.Restrict(names, p))
        }
      case Syntax.ProcessName(l) =>
        localSemantics(procEnv)(
          procEnv.getOrElse(
            l.name,
            Syntax.NullProcess())
        )

