package io.equiv.panproc.meta

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.*

object MetaSyntax:

  abstract sealed class MetaExpression

  /**
    * This class is meant to be used to hook in own kinds of judgments (e.g. typing in a context etc.)
    */
  abstract class MetaJudgment extends MetaExpression

  case class MetaRule(
    name: String,
    premisses: List[MetaExpression],
    conclusion: MetaExpression
  ) extends MetaExpression

  abstract case class MetaVariable(name: String) extends Intermediate:
    def pretty: String = name
    def prettyTex: String = name
