package io.equiv.panproc.lambda

import io.equiv.panproc.ts.{AbstractOperationalSemantics}
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.PatternMatching

object CallByValueSimpSemantics:
  abstract class EdgeLabel
  case class Step() extends EdgeLabel:
    override def toString(): String = "*"

  type NodeLabel = String

  val emptyEnv = Map[String,String]()

class CallByValueSimpSemantics(expr: Expression)
    extends AbstractOperationalSemantics[
      Expression,
      Map[String,String],
      Expression,
      CallByValueSimpSemantics.EdgeLabel,
      CallByValueSimpSemantics.NodeLabel
    ](expr):

  import CallByValueSimpSemantics.*

  override def stateIds(ex: Expression) = ex

  override def stateLabel(ex: Expression) = ex.pretty

  override def globalEnvironment(expr: Expression): (Map[String,String], Iterable[Expression]) =
    (emptyEnv, List(expr))

  def isValue(e: Expression) = e match
    case _: Lambda | _: Variable => true
    case _          => false

  override def localSemantics(env: Map[String,String])(e: Expression): Iterable[(EdgeLabel, Expression)] =
    e match
      case Application(Lambda(Variable(name), funTerm), argument) if isValue(argument) =>
        List(
          (Step(), funTerm.substituteAll(Map(name -> argument)))
        )
      case Application(function, argument) =>
        ( for
            (step, newFun) <- localSemantics(env)(function)
          yield
            (step, Application(newFun, argument))
        ) ++ (
          for
            (step, newArg) <- localSemantics(env)(argument)
          yield
            (step, Application(function, newArg))
        )
      case otherTerm =>
        List()
