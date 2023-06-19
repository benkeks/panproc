package io.equiv.panproc.game

trait SimpleGame:

  type GameNode = SimpleGame.GameNode
  type AttackerNode = SimpleGame.AttackerNode
  type DefenderNode = SimpleGame.DefenderNode

  def successors(gn: GameNode): Iterable[GameNode]

  def predecessors(gn: GameNode): Iterable[GameNode]

object SimpleGame:

  abstract class GameNode
  abstract class AttackerNode extends GameNode
  abstract class DefenderNode extends GameNode
