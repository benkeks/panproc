package io.equiv.panproc.game

abstract class GameGraphVisualizer(game: Game with GameDiscovery):

  def nodeToID(gn: Game.GameNode): String
  def nodeToString(gn: Game.GameNode): String
  def edgeToLabel(gn1: Game.GameNode, gn2: Game.GameNode): String

  def outputDot(attackerWinningRegion: Set[Game.GameNode]): String =

    val edgeOutput =
      for
        node <- game.discovered
        successor <- game.successors(node)
        color = "black"
        label = "\"" + edgeToLabel(node, successor) + "\""
      yield nodeToID(node) + "->" + nodeToID(successor) + s"[color=$color, label=$label]"

    val nodeOutput =
      for
        node <- game.discovered
        nodeID = nodeToID(node)
        nodeLabel = nodeToString(node)
        isAttacker = node.isInstanceOf[Game.AttackerNode]
        shape = if isAttacker then "square" else "circle, width=2, fixedsize=true"
        color = if attackerWinningRegion(node) then "red" else "blue"
        style = "bold"
      yield s"$nodeID [shape=$shape, color=$color, style=$style, label=" + "\"" + nodeLabel + "\"]"

    "digraph rel{" +
      nodeOutput.mkString("", ";", ";") +
      edgeOutput.mkString(";") +
      "}"
