package io.equiv.panproc.game

trait ReachabilityGame extends SimpleGame with GameLazyDecision[Boolean]:

  val initialPositions: Iterable[SimpleGame.GameNode]

  def computeSuccessors(gn: SimpleGame.GameNode): Iterable[SimpleGame.GameNode]

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
      case dn: DefenderNode if successors(node).isEmpty =>
        WinPrice
      case _ =>
        None

  populateGame(
    initialPositions,
    computeSuccessors(_),
    instantAttackerWin(_))
