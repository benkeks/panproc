package io.equiv.panproc.lambda

import io.equiv.panproc.ts.{AbstractOperationalSemantics}
import io.equiv.panproc.lambda.Syntax.*

object CallByValueBigStepSemantics:
  abstract class EdgeLabel
  case class BigStep() extends EdgeLabel:
    override def toString(): String = "*"

  type NodeLabel = String

  class Environment(defs: => Map[String, Expression], parent: Environment):

    def push(newDefs: => List[(String, Expression)]): Environment =
      Environment(flatten(newDefs.toMap), null)

    def push(otherEnvironment: Environment): Environment =
      Environment(otherEnvironment.flatten(defs), null)

    def get(x: String): Option[Expression] =
      val v = defs.get(x)
      if parent ne null then
        v.orElse(parent.get(x))
      else
        v

    def height(): Int =
      if parent == null then
        1
      else
        1 + parent.height()

    def flatten(collected: Map[String, Expression] = Map()): Map[String, Expression] =
      val relevantDefs = defs -- collected.keys
      val newCollected = collected ++ relevantDefs
      if parent == null then
        newCollected
      else
        parent.flatten(newCollected)

    override def hashCode(): Int = flatten().keySet.hashCode()

    override def equals(other: Any): Boolean =
      other match
        case e: Environment =>
          e.flatten().keySet == this.flatten().keySet
        case _ =>
          false

    override def toString() = height() + ":" + defs.keys.mkString(",")
  end Environment

  val emptyEnv = Environment(Map(), null)

  case class Bind(env: Environment, term: Expression) extends Intermediate:
    override def pretty = s"{${term.pretty}}"

    def unpacked(): Expression = term match
      case b: Bind => b.unpacked()
      case other   => other

class CallByValueBigStepSemantics(expr: Expression)
    extends AbstractOperationalSemantics[
      Expression,
      CallByValueBigStepSemantics.Environment,
      Expression,
      CallByValueBigStepSemantics.EdgeLabel,
      CallByValueBigStepSemantics.NodeLabel
    ](expr):

  import CallByValueBigStepSemantics.*

  override def stateIds(ex: Expression) = ex

  override def stateLabel(ex: Expression) = ex.pretty

  override def globalEnvironment(expr: Expression): (Environment, Iterable[Expression]) =
    (emptyEnv, List(expr))

  private def mkRec(env: Environment, defs: List[Definition]): Environment =
    lazy val rr = mkRec(env, defs)
    env.push {
      for
        Definition(Name(variable), value) <- defs
      yield (variable, Bind(rr, value))
    }

  def isValue(e: Expression) = e match
    case _: Literal => true
    case _: Bind => true
    case _ => false

  override def localSemantics(env: Environment)(e: Expression): Iterable[(EdgeLabel, Expression)] =
    println("lambda rule for: " + e + " in " + env)
    for
      (step, result) <- e match
        case Variable(variable) =>
          for
            v <- env.get(variable.name).toList
          yield BigStep() -> v
        case el @ Lambda(variables, term) =>
          List(BigStep() -> Bind(env, el))
        case Application(function, argument) =>
          for
            (BigStep(), Bind(funEnv, Lambda(funVar, funTerm))) <- localSemantics(env)(function)
            (BigStep(), argValue) <- localSemantics(env)(argument)
            callEnv = funEnv.push(List(funVar.name -> argValue))
            callStep <- localSemantics(callEnv)(funTerm)
          yield callStep
        case LetRec(definitions, in) =>
          localSemantics(mkRec(env, definitions))(in)
        case Bind(cEnv, term) =>
          localSemantics(cEnv)(term)
        case otherTerm =>
          // literals and open terms have loops in the reduction relation
          List(BigStep() -> otherTerm)
    yield
      if isValue(result) then
        (step -> result)
      else
        // if we dont reach a value, we freeze the execution, including the bindings.
        (step -> Bind(env, result))
