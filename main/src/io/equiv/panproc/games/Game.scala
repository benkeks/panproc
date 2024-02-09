package io.equiv.panproc.game

trait Game:

  type GameNode = Game.GameNode
  type AttackerNode = Game.AttackerNode
  type DefenderNode = Game.DefenderNode

  def successors(gn: GameNode): Iterable[GameNode]

  def predecessors(gn: GameNode): Iterable[GameNode]

object Game:

  abstract class GameNode
  abstract class AttackerNode extends GameNode
  abstract class DefenderNode extends GameNode
