package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.lambda.CallByValueBigStepSemantics
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.ccs.Syntax.ProcessExpression

object Semantics:
  case class Action(action: Syntax.Label) extends CallByValueBigStepSemantics.EdgeLabel:
    override def toString(): String = action.toString()
  case class CommunicationStep() extends CallByValueBigStepSemantics.EdgeLabel:
    override def toString(): String = "τ"

class Semantics(mainExpr: Expression)
    extends CallByValueBigStepSemantics(mainExpr):

  import Semantics.*
  import CallByValueBigStepSemantics.*

  override def localSemantics(env: Environment)(e: Expression): Iterable[(EdgeLabel, Expression)] =
    println("CCS rule for: " + e + " in " + env)
    for
      (step, result) <- e match
        case Syntax.Prefix(l, proc) =>
          List((Action(l), proc))
        case Syntax.Choice(procs) =>
          procs.flatMap(localSemantics(env)(_))
        case Syntax.Parallel(procs) =>
          val initialSteps = procs.map(localSemantics(env)(_))
          val initialStepsGrouped =
            initialSteps.map(_.groupBy { _._1 }).zipWithIndex
          val newSyncSteps =
            for
              (initsP, iP) <- initialStepsGrouped
              (initsQ, iQ) <- initialStepsGrouped
              if iP != iQ
              (Action(Syntax.Send(a)), pContAs) <- initsP
              qContAr <- initsQ.get(Action(Syntax.Receive(a))).toList
              (_, pAs) <- pContAs
              (_, qAr) <- qContAr
              newProcs = procs.zipWithIndex.map { case (p, i) =>
                if i == iP then
                  pAs
                else if i == iQ then
                  qAr
                else
                  p
              }
            yield CommunicationStep() -> Syntax.Parallel(newProcs)
          val newInterleavedSteps =
            for
              (initsP, iP) <- initialSteps.zipWithIndex
              (a, p) <- initsP
              newProcs = procs.updated(iP, p)
            yield a -> Syntax.Parallel(newProcs)
          newSyncSteps ++ newInterleavedSteps
        case Syntax.Restrict(names, proc) =>
          for
            (a, p) <- localSemantics(env)(proc)
            if a match
              case Action(Syntax.Send(name)) =>
                !names.contains(name)
              case Action(Syntax.Receive(name)) =>
                !names.contains(name)
              case _ =>
                true
          yield a -> Syntax.Restrict(names, p)
        case other =>
          super.localSemantics(env)(other)
      (finishingStep, finalizedResult) <-
        if isValue(result) then
          List(step -> result)
        else
          super.localSemantics(env)(result).filter(_._1.isInstanceOf[CallByValueBigStepSemantics.BigStep])
    yield
      (step, finalizedResult)

