package io.equiv.panproc.hml

import io.equiv.panproc.hml.HennessyMilnerLogic.*
import io.equiv.panproc.ts.WeakTransitionSystem
import io.equiv.panproc.meta.MetaSyntax.*

class HMLRules[S, A, L](val ts: WeakTransitionSystem[S, A, L]):

  case class HMLSatisfies(s: S, formula: Formula[A]) extends MetaJudgment
  case class LTSStep(s0: S, a: A, s1: S) extends MetaJudgment
  case class LTSInternalStep(s0: S, s1: S) extends MetaJudgment

  def obsI(s: S, obs: Observe[A]) =
    for s1 <- ts.post(s, obs.action).toList
    yield MetaRule("obsI", List(), LTSStep(s, obs.action, s1))

  def conjI(s: S, conj: And[A]) = List(MetaRule(
    "conjI",
    for c <- conj.subterms.toList yield HMLSatisfies(s, c),
    HMLSatisfies(s, conj)
  ))

  def disjI(s: S, disj: Negate[A]) = disj match
    case Negate(And(subterms)) =>
      for c <- subterms.toList yield MetaRule(
        "disjI",
        List(HMLSatisfies(s, Negate(c))),
        HMLSatisfies(s, disj)
      )
    case _ => List()

  def negObsI(s: S, negObs: Negate[A]) = negObs match
    case Negate(Observe(action, andThen)) =>
      List(MetaRule(
        "negObsI",
        for s1 <- ts.post(s, action).toList
          yield HMLSatisfies(s1, Negate(andThen)),
        HMLSatisfies(s, negObs)
      ))
    case _ => List()

