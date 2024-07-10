package io.equiv.panproc.hml

import io.equiv.panproc.hml.HennessyMilnerLogic.*
import io.equiv.panproc.ts.WeakTransitionSystem
import io.equiv.panproc.meta.MetaSyntax.*

class HMLRules[S, A]:

  def HMLSatisfies(p: MetaValuable[S], formula: MetaValuable[Formula[MetaValuable[A]]]) =
    MetaJudgment("hml_satisfies", List(p, formula))
  def LTSStep(p0: MetaValuable[S], a: MetaValuable[A], p1: MetaValuable[S]) =
    MetaJudgment("lts_step", List(p0, a, p1))
  def LTSInternalStep(p0: MetaValuable[S], p1: MetaValuable[S]) =
    MetaJudgment("lts_internal_step", List(p0, p1))

  val obsI = MetaRule("obsI",
    List(
      LTSStep(MetaVariable[S]("p"), MetaVariable[A]("a"), MetaVariable[S]("p'")),
      HMLSatisfies(MetaVariable[S]("p'"), MetaVariable("𝜑"))
    ),
    HMLSatisfies(MetaVariable[S]("p"), MetaFactory2(
      MetaVariable("a"), MetaVariable("a").asInstanceOf[MetaValuable[A]],
      MetaVariable("𝜑"), Placeholder[MetaValuable[A]]("𝜑"), (a, 𝜑) => Observe(a, 𝜑))
    )
  )

  def conjI[I](indices: List[I]) =
    MetaRule("conjI",
      indices.map(i =>
        HMLSatisfies(MetaVariable[S]("p"), MetaVariable(s"𝜑_$i"))
      ),
      HMLSatisfies(MetaVariable[S]("p"), MetaFactoryN(
        indices.map(i => MetaVariable(s"𝜑_$i")),
        indices.map(i => Placeholder(s"𝜑_$i").asInstanceOf[Formula[MetaValuable[A]]]),
        (𝜑s) => And(𝜑s.toSet))
      )
  )

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

