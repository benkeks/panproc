package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.lambda.CallByValueSemantics
import io.equiv.panproc.lambda.Syntax.Expression

object Semantics:
  case class Action(action: Syntax.Label) extends CallByValueSemantics.EdgeLabel

class Semantics(mainExpr: Expression)
    extends CallByValueSemantics(mainExpr):

  import Semantics.*
  import CallByValueSemantics.*

  override def localSemantics(env: Environment)(e: Expression)
      : List[(EdgeLabel, Expression)] =
    e match
      case Syntax.Prefix(l, proc) =>
        List((Action(l), proc))
      case Syntax.Choice(procs) =>
        procs.flatMap(localSemantics(env)(_))
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
        for
          (a, p) <- localSemantics(env)(proc)
          if !aNames.contains(a)
        yield (a, Syntax.Restrict(names, p))
      case other =>
        super.localSemantics(env)(other)
