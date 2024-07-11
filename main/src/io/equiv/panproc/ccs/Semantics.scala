package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.lambda.CallByValueBigStepSemantics
import io.equiv.panproc.lambda.Environment
import io.equiv.panproc.ccs.Syntax.*
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.PatternMatching
import io.equiv.panproc.lambda.CallByValueBigStepSemantics.Bind

object Semantics:
  abstract class ActionStep extends CallByValueBigStepSemantics.EdgeLabel

  case class SendStep(payload: Expression) extends ActionStep:
    override def toString(): String = s"!${payload.pretty}"

  case class ReceiveStep(pattern: Pattern) extends ActionStep:
    override def toString(): String = s"${pattern.pretty}"

  case class CommunicationStep() extends ActionStep:
    override def toString(): String = "τ"

  /** A place in a term expecting to be filled. */
  case class Hole() extends Intermediate:
    override def pretty = "⋅"
    override def prettyTex = "\\cdot"

  def fillHole(e: Expression, withExpression: Expression): Expression = e match
    case Variable(name) => e
    case Lambda(variable, term) => Lambda(variable, fillHole(term, withExpression))
    case Application(function, argument) => Application(fillHole(function, withExpression), fillHole(argument, withExpression))
    case Hole() => withExpression
    case Send(emitted, continuation) => Send(fillHole(emitted, withExpression), fillHole(continuation, withExpression))
    case Receive(Lambda(variable, term)) => Receive(Lambda(variable, fillHole(term, withExpression)))
    case LetRec(definitions, in) => LetRec(definitions.map {
      case Definition(pattern, value) => Definition(pattern, fillHole(value, withExpression))
    }, fillHole(in, withExpression))
    case Choice(procs) => Choice(procs.map(fillHole(_, withExpression)))
    case Parallel(procs) => Parallel(procs.map(fillHole(_, withExpression)))
    case Restrict(names, proc) => Restrict(names, fillHole(proc, withExpression))
    case Bind(env, term) => Bind(env, fillHole(term, withExpression))
    case l: Literal => l
    case i: Intermediate => i

  def hasHole(e: Expression): Boolean = e match
    case Bind(env, term) => hasHole(term)
    case Lambda(variable, term) => hasHole(term)
    case Send(emitted, continuation) => hasHole(emitted) || hasHole(continuation)
    case Application(function, argument) => hasHole(function) || hasHole(argument)
    case Hole() => true
    case Receive(receiver) => hasHole(receiver)
    case Choice(procs) => procs.exists(hasHole(_))
    case LetRec(definitions, in) => definitions.exists(d => hasHole(d.value)) || hasHole(in)
    case Parallel(procs) => procs.exists(hasHole(_))
    case Restrict(names, proc) => hasHole(proc)
    case _ => false


class Semantics(mainExpr: Expression)
    extends CallByValueBigStepSemantics(mainExpr):

  import Semantics.*
  import CallByValueBigStepSemantics.*

  override def localSemantics(env: Environment)(e: Expression): Iterable[(EdgeLabel, Expression)] =
    for
      (step: EdgeLabel, result: Expression) <- e match
        case Send(argument, continuation) =>
          for
            case (_, payload) <- super.localSemantics(env)(argument).take(1)
          yield SendStep(payload) -> continuation
        case Receive(Lambda(pattern, proc)) =>
          List(
            ReceiveStep(pattern) ->
              Application(Lambda(pattern, proc), Hole())
          )
        case Choice(procs) =>
          procs.flatMap(localSemantics(env)(_))
        case Parallel(procs) =>
          val initialSteps = procs.map(localSemantics(env)(_))
          val initialStepsGrouped =
            initialSteps.map(_.groupBy { _._1 }).zipWithIndex
          val newSyncSteps: Iterable[(EdgeLabel, Expression)] =
            for
              (initsP, iP) <- initialStepsGrouped
              (initsQ, iQ) <- initialStepsGrouped
              if iP != iQ
              case (SendStep(payload), pContAs) <- initsP
              qContAr <- initsQ.view.collect {
                case (ReceiveStep(pattern), continuation) if PatternMatching.patternCanMatch(pattern, payload) => continuation
              }
              (_, pAs) <- pContAs
              (_, qAr) <- qContAr
              newProcs = procs.zipWithIndex.map { case (p, i) =>
                if i == iP then
                  pAs
                else if i == iQ then
                  fillHole(qAr, payload)
                else
                  p
              }
            yield CommunicationStep() -> Parallel(newProcs)
          val newInterleavedSteps =
            for
              (initsP, iP) <- initialSteps.zipWithIndex
              (a, p) <- initsP
              newProcs = procs.updated(iP, p)
            yield a -> Parallel(newProcs)
          (newSyncSteps ++ newInterleavedSteps)
        case Restrict(restrictedPatterns, proc) =>
          for
            (a, p) <- localSemantics(env)(proc)
            if a match
              case SendStep(payload) =>
                !restrictedPatterns.exists(PatternMatching.patternCanMatch(_, payload))
              case ReceiveStep(pattern: Expression) =>
                !restrictedPatterns.exists(PatternMatching.patternCanMatch(_, pattern))
              case _ =>
                true
          yield a -> Restrict(restrictedPatterns, p)
        case Hole() =>
          List(BigStep() -> Hole())
        case other =>
          super.localSemantics(env)(other)
      (finishingStep, finalizedResult) <-
        if isValue(result) || hasHole(result) then
          List(step -> result)
        else
          super.localSemantics(env)(result).filter(
            _._1.isInstanceOf[CallByValueBigStepSemantics.BigStep]
          )
    yield (step, finalizedResult)
