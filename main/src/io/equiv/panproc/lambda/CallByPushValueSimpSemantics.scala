package io.equiv.panproc.lambda

import io.equiv.panproc.ts.{AbstractOperationalSemantics}
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.PatternMatching

object CallByPushValueSimpSemantics:
  abstract class EdgeLabel
  case class Step() extends EdgeLabel:
    override def toString(): String = "*"

  type NodeLabel = String

  val emptyEnv = Map[String,String]()

  case class Bind(monad: Expression, lambda: Lambda) extends Intermediate:
    override def pretty = s"${monad.pretty} >>= ${lambda.pretty}"
    override def prettyTex = s"${monad.prettyTex} >>= ${lambda.prettyTex}"
    override def freeVariables: Set[String] = monad.freeVariables ++ lambda.freeVariables
    override def substituteAll(fillIns: Map[String, Expression]): Bind =
      Bind(monad.substituteAll(fillIns), lambda.substituteAll(fillIns))

  case class Force(inner: Expression) extends Intermediate:
    override def pretty = s"(force ${inner.pretty})"
    override def prettyTex = s"(force ${inner.prettyTex})"
    override def freeVariables: Set[String] = inner.freeVariables
    override def substituteAll(fillIns: Map[String, Expression]): Force =
      Force(inner.substituteAll(fillIns))

  case class Return(inner: Expression) extends Intermediate:
    override def pretty = s"return ${inner.pretty}"
    override def prettyTex = s"return ${inner.prettyTex}"
    override def freeVariables: Set[String] = inner.freeVariables
    override def substituteAll(fillIns: Map[String, Expression]): Return =
      Return(inner.substituteAll(fillIns))

  case class Thunk(inner: Expression) extends Intermediate:
    override def pretty = s"thunk ${inner.pretty}"
    override def prettyTex = s"thunk ${inner.prettyTex}"
    override def freeVariables: Set[String] = inner.freeVariables
    override def substituteAll(fillIns: Map[String, Expression]): Thunk =
      Thunk(inner.substituteAll(fillIns))

  def encodeCallByValue(expr: Expression): Expression =
    expr match
      case Lambda(variable, term) =>
        Return(Thunk(Lambda(variable, encodeCallByValue(term))))
      case Application(function, argument) =>
        Bind(encodeCallByValue(function),
          Lambda(Variable("_f"),
            Bind(
              encodeCallByValue(argument),
              Lambda(Variable("_x"), Application(Force(Variable("_f")), Variable("_x")))
            )
          )
        )
      case Variable(name) =>
        Return(Variable(name))
      case other =>
        Thunk(other)

class CallByPushValueSimpSemantics(expr: Expression)
    extends AbstractOperationalSemantics[
      Expression,
      Map[String,String],
      Expression,
      CallByPushValueSimpSemantics.EdgeLabel,
      CallByPushValueSimpSemantics.NodeLabel
    ](expr):

  import CallByPushValueSimpSemantics.*

  override def stateIds(ex: Expression) = ex

  override def stateLabel(ex: Expression) = ex.pretty

  override def globalEnvironment(expr: Expression): (Map[String,String], Iterable[Expression]) =
    (emptyEnv, List(expr))

  def isValue(e: Expression) = e match
    case _: Thunk | _: Variable => true
    case _          => false

  override def localSemantics(env: Map[String,String])(e: Expression): Iterable[(EdgeLabel, Expression)] =
    e match
      case Bind(Return(inner), Lambda(Variable(name), funTerm)) =>
        List(
          (Step(), funTerm.substituteAll(Map(name -> inner)))
        )
      case Bind(first, Lambda(Variable(name), funTerm)) =>
        (for 
            (step, newFirst) <- localSemantics(env)(first)
          yield
            (step, Bind(newFirst, Lambda(Variable(name), funTerm)))
        )
      case Application(Lambda(Variable(name), funTerm), argument) if isValue(argument) =>
        List(
          (Step(), funTerm.substituteAll(Map(name -> argument)))
        )
      case Application(function, argument) =>
        ( for
            (step, newFun) <- localSemantics(env)(function)
          yield
            (step, Application(newFun, argument))
        )
      case Force(Thunk(inner)) =>
        List(
          (Step(), inner)
        )
      case otherTerm =>
        List()
