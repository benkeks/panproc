package io.equiv.panproc.ccs

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.ccs.Syntax.*
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.PatternMatching

object PiSemanticsEarly:
  abstract class ActionStep

  type NodeLabel = String

  val emptyEnv = Map[String,String]()

  case class SendStep(payload: Expression) extends ActionStep:
    override def toString(): String = s"!${payload.pretty}"

  case class ReceiveStep(pattern: Pattern) extends ActionStep:
    override def toString(): String = s"${pattern.pretty}"

  case class CommunicationStep() extends ActionStep:
    override def toString(): String = "τ"

class PiSemanticsEarly(mainExpr: Expression) extends AbstractOperationalSemantics[
  Expression,
  Map[String,String],
  Expression,
  PiSemanticsEarly.ActionStep,
  PiSemanticsEarly.NodeLabel
](mainExpr):

  import PiSemanticsEarly.*

  override def stateIds(ex: Expression) = ex

  override def stateLabel(ex: Expression) = ex.pretty

  override def globalEnvironment(expr: Expression): (Map[String,String], Iterable[Expression]) =
    (emptyEnv, List(expr))

  def isValue(e: Expression) = e match
    case _: Literal => true
    case _          => false
    
  override def localSemantics(env: Map[String, String])(e: Expression): Iterable[(ActionStep, Expression)] =
    List()