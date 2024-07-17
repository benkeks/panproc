package io.equiv.panproc.game

import scala.compiletime.ops.boolean

trait ReachabilityGame(initialPositions: Iterable[Game.GameNode]) extends Game with GameLazyDecision[Boolean]:

  def computeSuccessors(gn: Game.GameNode): Iterable[Game.GameNode]

  private val WinPrice = Some(true)

  override def priceIsBetter(p1: Boolean, p2: Boolean): Boolean = p1 < p2

  override def priceIsBetterOrEq(p1: Boolean, p2: Boolean): Boolean = p1 <= p2

  override def computeCurrentPrice(node: GameNode): Iterable[Boolean] =
    node match
      case an: AttackerNode =>
        if successors(node).exists(s => attackerVictoryPrices(s).nonEmpty) then
          WinPrice
        else
          None
      case dn: DefenderNode =>
        if successors(node).forall(s => attackerVictoryPrices(s).nonEmpty) then
          WinPrice
        else
          None

  def instantAttackerWin(node: GameNode) =
    node match
      //FIXME: Currently, computeSuccessors is thus called twice in game graph exploration.
      case dn: DefenderNode if computeSuccessors(node).isEmpty =>
        WinPrice
      case _ =>
        None

  def attackerWinningRegion(): Set[Game.GameNode] =
    attackerVictoryPrices.filter(_._2.nonEmpty).keySet.toSet

  def attackerWins(gn: Game.GameNode): Boolean =
    isAttackerWinningPrice(gn, true)

  def defenderWins(gn: Game.GameNode): Boolean =
    !attackerWins(gn)

  populateGame(
    initialPositions,
    computeSuccessors(_),
    instantAttackerWin(_)
  )
