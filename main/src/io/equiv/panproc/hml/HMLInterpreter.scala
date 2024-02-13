package io.equiv.panproc.hml

import io.equiv.panproc.hml.HennessyMilnerLogic.*
import io.equiv.panproc.ts.WeakTransitionSystem
import io.equiv.panproc.game.ReachabilityGame
import io.equiv.panproc.game.Game

class HMLInterpreter[S, A, L](
    val ts: WeakTransitionSystem[S, A, L]
):

  case class HMLAttack(s: S, formula: Formula[A]) extends Game.AttackerNode
  case class HMLDefense(s: S, formula: Formula[A]) extends Game.DefenderNode

  class HMLFormulaGame(formula: Formula[A], states: Iterable[S])
      extends ReachabilityGame:

    override val initialPositions: Iterable[Game.GameNode] =
      for s <- states yield makeNode(s, formula)

    def makeNode(s: S, formula: Formula[A]) = formula match
      case Observe(_, _) | ObserveInternal(_, _) | Negate(And(_)) | Pass(_) =>
        HMLDefense(s, formula)
      case And(_) | Negate(_) =>
        HMLAttack(s, formula)

    override def computeSuccessors(gn: Game.GameNode): Iterable[Game.GameNode] = gn match
      case HMLAttack(s, And(subterms)) =>
        for
          f <- subterms
        yield makeNode(s, f)
      case HMLAttack(s, Negate(Observe(action, andThen))) =>
        for
          s1 <- ts.post(s, action)
        yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(ObserveInternal(andThen, opt))) =>
        for
          s1 <- ts.silentSteps.values(s) ++ (if opt then Some(s) else None)
        yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(Pass(andThen))) =>
        for
          s1 <- ts.silentReachable(s)
        yield makeNode(s1, Negate(andThen))
      case HMLAttack(s, Negate(Negate(andThen))) =>
        List(makeNode(s, andThen))
      case HMLDefense(s, Negate(And(subterms))) =>
        for
          f <- subterms
        yield makeNode(s, Negate(f))
      case HMLDefense(s, Observe(action, andThen)) =>
        for
          s1 <- ts.post(s, action)
        yield makeNode(s1, andThen)
      case HMLDefense(s, ObserveInternal(andThen, opt)) =>
        for
          s1 <- ts.silentSteps.values(s) ++ (if opt then Some(s) else None)
        yield makeNode(s1, andThen)
      case HMLDefense(s, Pass(andThen)) =>
        for
          s1 <- ts.silentReachable(s)
        yield makeNode(s1, andThen)

  def isTrueAt(f: Formula[A], states: Iterable[S]): Map[S, Boolean] =
    val interpretationGame = new HMLFormulaGame(f, states)
    val result =
      for
        s <- states
      yield s ->
        interpretationGame.attackerVictoryPrices(interpretationGame.makeNode(s, f)).nonEmpty
    result.toMap
