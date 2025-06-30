package io.equiv.panproc.meta

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.lambda.PatternMatching

object MetaSyntax:

  abstract sealed class MetaExpression {
    def pretty: String
    def prettyTex: String
    def substituteAll(substitution: Map[String, Expression]): MetaExpression
  }

  case class MetaJudgment(name: String, parameters: List[Expression]) extends MetaExpression {
    def pretty: String = s"$name(${parameters.map(_.pretty).mkString(", ")})"
    def prettyTex: String = s"$name(${parameters.map(_.prettyTex).mkString(", ")})"
    def isClosed() = parameters.forall(_.isClosed())

    def substituteAll(substitution: Map[String, Expression]) =
      MetaJudgment(name, parameters.map(_.substituteAll(substitution)))

    def matchJudgment(otherJudgment: MetaJudgment): Option[Map[String, Expression]] =
      if (this.name == otherJudgment.name && this.parameters.length == otherJudgment.parameters.length)
        val leftPatterns = this.parameters.map(_ match
            case p: Pattern => Some(p)
            case appl: Application => Some(appl.toPattern())
            case _ => None
        )
        if (leftPatterns.forall(_.isDefined))
          var environment: Option[Map[String, Expression]] = Some(Map[String, Expression]())
          for (case (Some(leftPattern), right) <- leftPatterns.zip(otherJudgment.parameters))
            environment = environment.flatMap{ env =>
              val rightInstantiated = right.substituteAll(env)
              if (PatternMatching.patternCanMatch(leftPattern, rightInstantiated, variableNameMatching = false))
                Some(env ++ PatternMatching.matchPattern(leftPattern, rightInstantiated, variableNameMatching = false))
              else
                None
            }
          environment
        else
          None
      else
        None
  }

  case class MetaRule(
    name: String,
    premisses: List[MetaExpression],
    conclusion: MetaExpression
  ) extends MetaExpression {
    def pretty: String = premisses.map(_.pretty).mkString("\n") + "\n [" + name + "] --------------\n" + conclusion.pretty
    def prettyTex: String = pretty

    def substituteAll(substitution: Map[String, Expression]): MetaExpression =
      MetaRule(name, premisses.map(_.substituteAll(substitution)), conclusion.substituteAll(substitution))

    def backwardsStep(goal: MetaJudgment): Option[List[MetaExpression]] =
      for
        assignment <- matchConclusion(goal)
      yield
        premisses.map(_.substituteAll(assignment))

    def matchConclusion(goal: MetaJudgment): Option[Map[String, Expression]] =
      conclusion match
        case conclusionJudgment: MetaJudgment =>
          conclusionJudgment.matchJudgment(goal)
        case _ => None
  }

  def MetaAxiom(name: String, conclusion: MetaExpression) = MetaRule(name, List(), conclusion)
