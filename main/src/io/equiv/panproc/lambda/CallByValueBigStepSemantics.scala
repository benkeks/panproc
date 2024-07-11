package io.equiv.panproc.lambda

import io.equiv.panproc.ts.{AbstractOperationalSemantics}
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.Environment
import io.equiv.panproc.lambda.PatternMatching

object CallByValueBigStepSemantics:
  abstract class EdgeLabel
  case class BigStep() extends EdgeLabel:
    override def toString(): String = "*"

  type NodeLabel = String

  val emptyEnv = Environment(Map(), null)

  case class Bind(env: Environment, term: Expression) extends Intermediate:
    override def pretty = s"{${term.pretty}}"
    override def prettyTex = s"{${term.prettyTex}}"

    def unpacked(): Expression = term match
      case b: Bind => b.unpacked()
      case other   => other

class CallByValueBigStepSemantics(expr: Expression)
    extends AbstractOperationalSemantics[
      Expression,
      Environment,
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
        case Definition(Variable(variable), value) <- defs
      yield (variable, Bind(rr, value))
    }

  def isValue(e: Expression) = e match
    case _: Literal => true
    case _: Bind    => true
    case _          => false

  override def localSemantics(env: Environment)(e: Expression): Iterable[(EdgeLabel, Expression)] =
    for
      (step, result) <- e match
        case Variable(variable) =>
          for
            v <- env.get(variable).orElse(Some(e)).toList
          yield BigStep() -> v
        case el @ Lambda(variables, term) =>
          List(BigStep() -> Bind(env, el))
        case Application(function, argument) =>
          for
            fun <- localSemantics(env)(function)
            step <- fun match
              case (BigStep(), Bind(funEnv, Lambda(pattern, funTerm))) =>
                for
                  case (BigStep(), argValue) <- localSemantics(env)(argument)
                  if PatternMatching.patternCanMatch(pattern, argValue)
                  variableMatching = PatternMatching.matchPattern(pattern, argValue)
                  callEnv = funEnv.push(variableMatching)
                  callStep <- localSemantics(callEnv)(funTerm)
                yield callStep
              case (BigStep(), pattern) =>
                for
                  case (BigStep(), argValue) <- localSemantics(env)(argument)
                yield
                  (BigStep(), Application(pattern, argValue))
          yield step
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
        (step -> Bind(env, result)
      )
