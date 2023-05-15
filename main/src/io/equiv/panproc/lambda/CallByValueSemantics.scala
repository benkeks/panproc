package io.equiv.panproc.lambda

import io.equiv.panproc.ts.{AbstractOperationalSemantics, DerivedBigSteps}
import io.equiv.panproc.lambda.Syntax.*

object CallByValueSemantics:
  abstract class EdgeLabel
  case class InternalStep() extends EdgeLabel:
    override def toString(): String = "τ"

  type NodeLabel = String

  // call-by-name defs parameter allows for gfp-style mkRec.
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

  case class Bind(env: Environment, term: Syntax.Expression) extends Syntax.Intermediate:
    override def pretty = s"{${term.pretty}}"

    def unpacked(): Syntax.Expression = term match
      case b: Bind => b.unpacked()
      case other => other


class CallByValueSemantics(expr: Syntax.Expression)
    extends AbstractOperationalSemantics[
      Syntax.Expression,
      CallByValueSemantics.Environment,
      Syntax.Expression,
      CallByValueSemantics.EdgeLabel,
      CallByValueSemantics.NodeLabel
    ](expr)
    with DerivedBigSteps[
      Syntax.Expression,
      CallByValueSemantics.Environment,
      CallByValueSemantics.EdgeLabel,
      CallByValueSemantics.NodeLabel
    ]:

  import CallByValueSemantics.*

  override def stateIds(ex: Syntax.Expression) = ex

  override def stateLabel(ex: Syntax.Expression) = ex.pretty

  override def globalEnvironment(expr: Syntax.Expression)
      : (Environment, Iterable[Syntax.Expression]) =
    (emptyEnv, List(expr))

  override def isValue(e: Syntax.Expression): Boolean =
    e match
      case Bind(_, Lambda(_, _)) => true // closures count as values
      case LetRec(_, in)         => isValue(in) // computed letrecs count as values
      case l: Literal            => true
      case _                     => false

  private def mkRec(env: Environment, defs: List[Definition]): Environment =
    lazy val rr = mkRec(env, defs)
    env.push {
      for
        Definition(Name(variable), value) <- defs
      yield (variable, Bind(rr, value))
    }

  override def localSemantics(env: Environment)(e: Syntax.Expression)
      : Iterable[(EdgeLabel, Syntax.Expression)] =
    println("lambda rule for: " + e)
    e match
      case el @ Lambda(variables, term) =>
        // bake lambdas into closures
        List(InternalStep() -> Bind(env, el))
      case Variable(variable) =>
        for
          expr <- env.get(variable.name).toList
        yield InternalStep() -> expr
      case Application(Bind(cEnv, Lambda(variable, term)), argument) if isValue(argument) =>
        List(
          InternalStep() -> Bind(cEnv.push(List(variable.name -> argument)), term)
        )
      case Application(function, argument) if isValue(function) =>
        for
          (l, e) <- this.localSemantics(env)(argument)
        yield l -> Application(function, e)
      case Application(function, argument) =>
        for
          (l, e) <- this.localSemantics(env)(function)
        yield (l -> Application(e, argument))
      case LetRec(definitions, in) =>
        for
          (l, e) <- this.localSemantics(mkRec(env, definitions))(in)
        yield (l, LetRec(definitions, e))
      case Bind(env0, Bind(env1, term)) =>
        List(InternalStep() -> Bind(env0.push(env1), term))
      case Bind(env0, term) if isValue(term) =>
        List(InternalStep() -> term)
      case Bind(env0, Lambda(_, _)) =>
        List()
      case Bind(env0, term) =>
        for
          (l, e) <- this.localSemantics(env0)(term)
        yield (l, Bind(env0, e))
      case _ => List()
