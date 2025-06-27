package io.equiv.panproc.meta

import io.equiv.panproc.meta.MetaSyntax.*
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.game.ReachabilityGame
import io.equiv.panproc.game.Game.{AttackerNode, DefenderNode, GameNode}
import io.equiv.panproc.game.GameGraphVisualizer

trait MetaDerivation(judgment: MetaJudgment):

  val metaRules: List[MetaRule]

  def suggestInstantiation(metaExpression: MetaExpression): Iterable[Map[String, Expression]]

  case class ProverChoice(judgment: MetaJudgment) extends AttackerNode
  case class DisproverChoice(options: List[MetaJudgment]) extends DefenderNode

  class DerivationGame extends ReachabilityGame(List(ProverChoice(judgment))):

    override def computeSuccessors(gn: GameNode): Iterable[GameNode] =
      gn match
        case ProverChoice(judgment) =>
          for
            rule <- metaRules
            applicationSchema <- rule.backwardsStep(judgment).toList
            //if applicationSchema.forall(_.isInstanceOf[MetaJudgment])
            premises = applicationSchema.map(_.asInstanceOf[MetaJudgment])
            instantiations = premises.flatMap(p => suggestInstantiation(p))
            instantiationVariant <- instantiations ++ List(Map())
            instantiatedPremises = premises.map(_.substituteAll(instantiationVariant).asInstanceOf[MetaJudgment])
          yield
            DisproverChoice(instantiatedPremises).asInstanceOf[GameNode]
        case DisproverChoice(options) =>
          options.map(ProverChoice(_))
        case _: GameNode => Nil

    val visualizer = new GameGraphVisualizer(this):

      def nodeToID(gn: GameNode): String = gn.toString().hashCode().toString()

      def nodeToString(gn: GameNode): String =
        gn match
          case ProverChoice(judgment) =>
            s"$judgment"
          case DisproverChoice(options) =>
            s"$options"
          case _ => ""

      def edgeToLabel(gn1: GameNode, gn2: GameNode) =
        "" // TODO: save rules? game.weight(gn1, gn2).toString()

    def asGraphvizString() =
      visualizer.outputDot(this.attackerWinningRegion())

  lazy val game = new DerivationGame()

  def hasProof() =
    game.attackerWins(ProverChoice(judgment))
