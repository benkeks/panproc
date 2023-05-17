package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.lambda.CallByValueBigStepSemantics
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.lambda.Environment
import io.equiv.panproc.ccs.Syntax.ProcessExpression
import io.equiv.panproc.lambda.Syntax.Application

object Semantics:
  abstract class ActionStep extends CallByValueBigStepSemantics.EdgeLabel

  case class SendStep(name: Syntax.Name, payload: Option[Expression]) extends ActionStep:
    override def toString(): String = payload match
      case None        => s"$name!⟨⟩"
      case Some(value) => s"$name!⟨${value.pretty}⟩"

  case class ReceiveStep(name: Syntax.Name) extends ActionStep:
    override def toString(): String = s"$name()"

  case class CommunicationStep() extends ActionStep:
    override def toString(): String = "τ"

class Semantics(mainExpr: Expression)
    extends CallByValueBigStepSemantics(mainExpr):

  import Semantics.*
  import CallByValueBigStepSemantics.*

  override def localSemantics(env: Environment)(e: Expression): Iterable[(EdgeLabel, Expression)] =
    for
      (step: EdgeLabel, result: Expression) <- e match
        case Syntax.Send(Syntax.Label(name, argument), proc) =>
          val arg =
            for
              a <- argument
              (_, value) <- super.localSemantics(env)(a).headOption
            yield value
          List(SendStep(name, arg) -> proc)
        case Syntax.Receive(Syntax.Label(name, _), proc) =>
          List(ReceiveStep(name) -> proc)
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
              case (SendStep(a, payload), pContAs) <- initsP
              qContAr <- initsQ.get(ReceiveStep(a)).toList
              (_, pAs) <- pContAs
              (_, qAr) <- qContAr
              newProcs = procs.zipWithIndex.map { case (p, i) =>
                if i == iP then
                  pAs
                else if i == iQ then
                  payload match
                    case None       => qAr
                    case Some(data) => Application(qAr, data)
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
              case SendStep(name, _) =>
                !names.contains(name)
              case ReceiveStep(name) =>
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
          super.localSemantics(env)(result).filter(
            _._1.isInstanceOf[CallByValueBigStepSemantics.BigStep]
          )
    yield (step, finalizedResult)
