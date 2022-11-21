package io.equiv.panproc.lambda

import io.equiv.panproc.ts.AbstractOperationalSemantics
import io.equiv.panproc.lambda.Syntax.*

object CallByValueSemantics:
  type EdgeLabel = Unit
  type NodeLabel = Unit

  // call-by-name defs parameter allows for gfp-style mkRec.
  protected class Environment(defs: => Map[String, Expression], parent: Environment):

    def push(newDefs: => List[(String, Expression)]): Environment =
      Environment(newDefs.toMap, this)

    def get(x: String): Option[Expression] =
      val v = defs.get(x)
      if parent ne null then
        v.orElse(parent.get(x))
      else
        v

    def flatten(collected: Map[String, Expression] = Map()): Map[String, Expression] =
      val relevantDefs = defs -- collected.keys
      val newCollected = collected ++ relevantDefs
      if parent == null then
        newCollected
      else
        parent.flatten(newCollected)

    override def toString() = "e"
  end Environment

  case class Bind(env: Environment, term: Syntax.Expression) extends Syntax.Intermediate:
    override def pretty = s"{${term.pretty}}"

class CallByValueSemantics(expr: Syntax.Expression)
    extends AbstractOperationalSemantics[
      Syntax.Expression,
      CallByValueSemantics.Environment,
      Syntax.Expression,
      CallByValueSemantics.EdgeLabel,
      CallByValueSemantics.NodeLabel
    ](expr):

  import CallByValueSemantics.*

  override def stateIds(ex: Syntax.Expression) = ex

  override def globalEnvironment(expr: Syntax.Expression)
      : (Environment, Iterable[Syntax.Expression]) =
    (Environment(Map(), null), List(expr))

  def isValue(e: Syntax.Expression) =
    e match
      case Bind(_, Lambda(_, _)) => true // closures count as values
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
      : List[(EdgeLabel, Syntax.Expression)] =
    e match
      case el @ Lambda(variables, term) =>
        // bake lambdas into closures
        List(() -> Bind(env, el))
      case Variable(variable) =>
        for
          expr <- env.get(variable.name).toList
        yield () -> expr
      case Application(Bind(cEnv, Lambda(variable, term)), argument) if isValue(argument) =>
        List(
          () -> Bind(cEnv.push(List(variable.name -> argument)), term)
        )
      case Application(function, argument) if isValue(function) =>
        for
          (l, e) <- localSemantics(env)(argument)
        yield l -> Application(function, e)
      case Application(function, argument) =>
        for
          (l, e) <- localSemantics(env)(function)
        yield (l -> Application(e, argument))
      case LetRec(definitions, in) =>
        //List(() -> Bind(mkRec(env, definitions), in))
        for
          (l, e) <- localSemantics(mkRec(env, definitions))(in)
        yield (l, LetRec(definitions, e))
      case Bind(env, term) if isValue(term) =>
        List(() -> term)
      case  Bind(env, Lambda(_, _)) =>
        List()
      case Bind(env, term) =>
        for
          (l, e) <- localSemantics(env)(term)
        yield (l, Bind(env, e))
      case _ => List()
