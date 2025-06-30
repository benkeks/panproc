package io.equiv.panproc.lambda

import io.equiv.panproc.ts.{AbstractOperationalSemantics}
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.PatternMatching

object CallByPushValueSimpSemantcs:
  abstract class EdgeLabel
  case class Step() extends EdgeLabel:
    override def toString(): String = "*"

  type NodeLabel = String

  val emptyEnv = Map[String,String]()

  case class Bind(monad: Expression, lambda: Lambda) extends Intermediate:
    override def pretty = s"${monad.pretty} >>= ${lambda.pretty}"
    override def prettyTex = s"${monad.prettyTex} >>= ${lambda.prettyTex}"
    override def freeVariables: Set[String] = monad.freeVariables ++ lambda.freeVariables

  case class Force(inner: Expression) extends Intermediate:
    override def pretty = s"force ${inner.pretty}"
    override def prettyTex = s"force ${inner.prettyTex}"
    override def freeVariables: Set[String] = inner.freeVariables

  case class Return(inner: Expression) extends Intermediate:
    override def pretty = s"return ${inner.pretty}"
    override def prettyTex = s"return ${inner.prettyTex}"
    override def freeVariables: Set[String] = inner.freeVariables

  case class Thunk(inner: Expression) extends Intermediate:
    override def pretty = s"thunk ${inner.pretty}"
    override def prettyTex = s"thunk ${inner.prettyTex}"
    override def freeVariables: Set[String] = inner.freeVariables

class CallByPushValueSimpSemantcs(expr: Expression)
    extends AbstractOperationalSemantics[
      Expression,
      Map[String,String],
      Expression,
      CallByPushValueSimpSemantcs.EdgeLabel,
      CallByPushValueSimpSemantcs.NodeLabel
    ](expr):

  import CallByPushValueSimpSemantcs.*

  override def stateIds(ex: Expression) = ex

  override def stateLabel(ex: Expression) = ex.pretty

  override def globalEnvironment(expr: Expression): (Map[String,String], Iterable[Expression]) =
    (emptyEnv, List(expr))

  def isValue(e: Expression) = e match
    case _: Thunk | _: Variable => true
    case _          => false

  override def localSemantics(env: Map[String,String])(e: Expression): Iterable[(EdgeLabel, Expression)] =
    e match
      case Bind(Return(inner), Lambda(Variable(name), funTerm)) if isValue(inner) =>
        List(
          (Step(), Syntax.substituteAll(funTerm, Map(name -> inner)))
        )
      case Application(Lambda(Variable(name), funTerm), argument) if isValue(argument) =>
        List(
          (Step(), Syntax.substituteAll(funTerm, Map(name -> argument)))
        )
      case Application(function, argument) =>
        ( for
            (step, newFun) <- localSemantics(env)(function)
          yield
            (step, Application(newFun, argument))
        )
      case otherTerm =>
        List()
