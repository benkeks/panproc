package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.lambda.CallByValueSemantics
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.ccs.Syntax.ProcessExpression

object Semantics:
  case class Action(action: Syntax.Label) extends CallByValueSemantics.EdgeLabel:
    override def toString(): String = action.toString()

class Semantics(mainExpr: Expression)
    extends CallByValueSemantics(mainExpr):

  import Semantics.*
  import CallByValueSemantics.*

  override def localSemantics(env: Environment)(e: Expression): Iterable[(EdgeLabel, Expression)] =
    e match
      case Bind(env, Syntax.Prefix(l, proc)) =>
        //TODO apply Bind
        List((Action(l), Bind(env, proc)))
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
          yield CallByValueSemantics.InternalStep() -> Syntax.Parallel(newProcs)
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
        for
          valueExpr <- eval(super.localSemantics(env), isProcessExpression)(other)
          (a, p) <-
            if valueExpr.isInstanceOf[Syntax.ProcessExpression] then
              localSemantics(env)(valueExpr)
            else
              super.localSemantics(env)(valueExpr)
        yield a -> p

  def isProcessExpression(e: Expression): Boolean = e match
    case b: Bind => isProcessExpression(b.unpacked())
    case pe: ProcessExpression => true
    case _ => false

