package io.equiv.panproc.meta

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.*

object MetaSyntax:

  abstract sealed class MetaExpression {
    def pretty: String
    def prettyTex: String
  }

  case class MetaJudgment(name: String, parameters: List[MetaValuable[_]]) extends MetaExpression {
    def pretty: String = s"$name(${parameters.map(_.pretty).mkString(", ")})"
    def prettyTex: String = s"$name(${parameters.map(_.prettyTex).mkString(", ")})"
  }

  abstract class MetaValuable[T] {
    def pretty: String
    def prettyTex: String
  }

  case class MetaVariable[T](name: String) extends MetaValuable[T]:
    def pretty: String = name
    def prettyTex: String = name


  case class MetaFactoryN[TI, TO](args: List[MetaValuable[TI]], placeholders: List[TI], factory: List[TI] => TO)
      extends MetaValuable[TO]:
    def pretty: String = factory(placeholders).toString()
    def prettyTex: String = factory(placeholders).toString()

  case class MetaFactory1[TI1, TO](args1: MetaValuable[TI1], placeholder1: TI1, factory: TI1 => TO)
      extends MetaValuable[TO]:
    def pretty: String = factory(placeholder1).toString()
    def prettyTex: String = factory(placeholder1).toString()

  case class MetaFactory2[TI1, TI2, TO](args1: MetaValuable[TI1], placeholder1: TI1, args2: MetaValuable[TI2], placeholder2: TI2,
    factory: (TI1, TI2) => TO)
      extends MetaValuable[TO]:
    def pretty: String = factory(placeholder1, placeholder2).toString()
    def prettyTex: String = factory(placeholder1, placeholder2).toString()


  case class MetaValue[T](value: T) extends MetaValuable[T]:
    def pretty: String = value.toString()
    def prettyTex: String = value.toString()

  case class MetaRule(
    name: String,
    premisses: List[MetaExpression],
    conclusion: MetaExpression
  ) extends MetaExpression {
    def pretty: String = premisses.map(_.pretty).mkString("\n") + "\n [" + name + "] --------------\n" + conclusion.pretty
    def prettyTex: String = pretty
  }
