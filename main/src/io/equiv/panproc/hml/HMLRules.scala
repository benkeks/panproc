package io.equiv.panproc.hml

import io.equiv.panproc.hml.HennessyMilnerLogic.*
import io.equiv.panproc.ts.TransitionSystem
import io.equiv.panproc.meta.MetaSyntax.*
import io.equiv.panproc.lambda.Syntax.*
import io.equiv.panproc.meta.MetaDerivation

class HMLRules[S, A, L](ts: TransitionSystem[S, A, L]):

  case class StateLiteral(s: S) extends Literal with DefaultPrettyPrinting(s)
  case class ActionLiteral(a: A) extends Literal with DefaultPrettyPrinting(a)
  case class ObservationOperator() extends Literal with DefaultPrettyPrinting("<>")
  case class ConjunctionOperator() extends Literal with DefaultPrettyPrinting("/\\")

  val obsI = MetaRule(
    "obsI",
    List(
      MetaJudgment("lts_step", List(Variable("p"), Variable("a"), Variable("p'"))),
      MetaJudgment("hml_satisfies", List(Variable("p'"), Variable("𝜑")))
    ),
    MetaJudgment(
      "hml_satisfies",
      List(
        Variable("p"),
        Application(Application(ObservationOperator(), Variable("a")), Variable("𝜑"))
      )
    )
  )

  val trueI =
    MetaAxiom("trueI", MetaJudgment("hml_satisfies", List(Variable("p"), ConjunctionOperator())))

  val conjI2 = MetaRule(
    "conjI2",
    List(
      MetaJudgment("hml_satisfies", List(Variable("p"), Variable("𝜑1"))),
      MetaJudgment("hml_satisfies", List(Variable("p"), Variable("𝜑2")))
    ),
    MetaJudgment(
      "hml_satisfies",
      List(
        Variable("p"),
        Application(Application(ConjunctionOperator(), Variable("𝜑1")), Variable("𝜑2"))
      )
    )
  )

  val ltsStepJudgments = ts.step.tupleSet.map { case (s0, a, s1) =>
    MetaJudgment("lts_step", List(StateLiteral(s0), ActionLiteral(a), StateLiteral(s1)))
  }
  val ltsSteps = ltsStepJudgments.map(MetaAxiom("lts", _))

  class HMLCheck(judgment: MetaJudgment) extends MetaDerivation(judgment):
    val metaRules = List(obsI, trueI, conjI2) ++ ltsSteps

    override def suggestInstantiation(metaExpression: MetaExpression)
        : Set[Map[String, Expression]] =
      metaExpression match
        case j @ MetaJudgment("lts_step", parameters) =>
          ltsStepJudgments.flatMap(j.matchJudgment(_))
        case _ => Set()

  def checkJudgment(judgment: MetaJudgment) =
    new HMLCheck(judgment).game.asGraphvizString()
