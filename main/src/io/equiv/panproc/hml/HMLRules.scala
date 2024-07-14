package io.equiv.panproc.hml

import io.equiv.panproc.hml.HennessyMilnerLogic.*
import io.equiv.panproc.ts.TransitionSystem
import io.equiv.panproc.meta.MetaSyntax.*
import io.equiv.panproc.lambda.Syntax.*

class HMLRules[S, A, L](ts: TransitionSystem[S, A, L]):

  case class StateLiteral(s: S) extends Literal with DefaultPrettyPrinting(s)
  case class ActionLiteral(a: A) extends Literal with DefaultPrettyPrinting(a)
  case class ObservationOperator() extends Literal with DefaultPrettyPrinting("<>")
  case class ConjunctionOperator() extends Literal with DefaultPrettyPrinting("/\\")

  val obsI = MetaRule("obsI",
    List(
      MetaJudgment("lts_step", List(Variable("p"), Variable("a"), Variable("p'"))),
      MetaJudgment("hml_satisfies", List(Variable("p'"), Variable("𝜑")))
    ),
    MetaJudgment("hml_satisfies", List(Variable("p"), Application(Application(ObservationOperator(), Variable("a")), Variable("𝜑"))))
  )

  val conjI2 = MetaRule("conjI2",
    List(
      MetaJudgment("hml_satisfies", List(Variable("p"), Variable("𝜑1"))),
      MetaJudgment("hml_satisfies", List(Variable("p"), Variable("𝜑2")))
    ),
    MetaJudgment("hml_satisfies", List(Variable("p"), Application(Application(ConjunctionOperator(), Variable("𝜑1")), Variable("𝜑2"))))
  )

  val ltsSteps = ts.step.tupleSet.map { case (s0, a, s1) => MetaAxiom("lts", MetaJudgment("lts_step", List(StateLiteral(s0), ActionLiteral(a), StateLiteral(s1))))}

  // def conjI[I](indices: List[I]) =
  //   MetaRule("conjI",
  //     indices.map(i =>
  //       HMLSatisfies(MetaVariable[S]("p"), MetaVariable(s"𝜑_$i"))
  //     ),
  //     HMLSatisfies(MetaVariable[S]("p"), MetaFactoryN(
  //       indices.map(i => MetaVariable(s"𝜑_$i")),
  //       indices.map(i => Placeholder(s"𝜑_$i").asInstanceOf[Formula[MetaValuable[A]]]),
  //       (𝜑s) => And(𝜑s.toSet))
  //     )
  // )

  // def conjI(s: S, conj: And[A]) = List(MetaRule(
  //   "conjI",
  //   for c <- conj.subterms.toList yield HMLSatisfies(s, c),
  //   HMLSatisfies(s, conj)
  // ))

  // def disjI(s: S, disj: Negate[A]) = disj match
  //   case Negate(And(subterms)) =>
  //     for c <- subterms.toList yield MetaRule(
  //       "disjI",
  //       List(HMLSatisfies(s, Negate(c))),
  //       HMLSatisfies(s, disj)
  //     )
  //   case _ => List()

  // def negObsI(s: S, negObs: Negate[A]) = negObs match
  //   case Negate(Observe(action, andThen)) =>
  //     List(MetaRule(
  //       "negObsI",
  //       for s1 <- ts.post(s, action).toList
  //         yield HMLSatisfies(s1, Negate(andThen)),
  //       HMLSatisfies(s, negObs)
  //     ))
  //   case _ => List()

