package io.equiv.panproc.lambda

object Syntax:

  abstract sealed class Expression:
    def apply(other: Expression) = Application(this, other)
    def pretty: String
    def prettyTex: String

  trait Pattern:
    def pretty: String
    def prettyTex: String

  case class Variable(name: String) extends Expression, Pattern:
    override def toString() = name
    def pretty: String = name
    def prettyTex: String = s"\\mathit{$name}"

  case class Constructor(left: Pattern, right: Pattern) extends Pattern:
    def pretty: String = s"(${left.pretty} ${right.pretty})"
    def prettyTex: String = s"(${left.prettyTex} ${right.prettyTex})"

  case class Lambda(val variable: Pattern, val term: Expression) extends Expression:
    override def pretty = s"(λ${variable.pretty}. ${term.pretty})"
    override def prettyTex = s"(\\lambda ${variable.prettyTex} \\ldotp  ${term.prettyTex})"

  case class Application(function: Expression, argument: Expression) extends Expression:
    override def pretty = s"${function.pretty} ${argument.pretty}"
    override def prettyTex = s"${function.prettyTex} \\left( ${argument.prettyTex} \\right)"

    def toPattern(): Pattern =
      Constructor(
        function match
          case p: Pattern =>
            p
          case a: Application =>
            a.toPattern()
          case _ =>
            throw Exception(s"$function cannot be a pattern.")
        ,
        argument match
          case p: Pattern =>
            p
          case a: Application =>
            a.toPattern()
          case _ =>
            throw Exception(s"$argument cannot be a pattern."),
      )

  case class Definition(pattern: Pattern, value: Expression):
    def pretty = s"${pattern.pretty} = ${value.pretty}"
    def prettyTex = s"${pattern.prettyTex} := ${value.prettyTex}"

  case class LetRec(definitions: List[Definition], in: Expression) extends Expression:
    override def pretty = s"letrec ${definitions.map(_.pretty).mkString("; ")} in ${in.pretty}"
    override def prettyTex = s"\\mathsf{let rec}\\; ${definitions.map(_.prettyTex).mkString("; \\; ")} \\;\\mathsf{in}\\; ${in.prettyTex}"

  abstract class Literal extends Expression, Pattern

  case class Number(number: Int) extends Literal:
    override def pretty: String = number.toString()
    override def prettyTex: String = pretty

  case class Unit() extends Literal:
    override def pretty: String = "()"
    override def prettyTex: String = "()"

  trait Intermediate extends Expression

  object Notation:

    given stringToVar: Conversion[String, Variable] with
      def apply(name: String): Variable = Variable(name)

    given intToNum: Conversion[Int, Number] with
      def apply(number: Int): Number = Number(number)

    given applicationToPattern: Conversion[Application, Pattern] with
      def apply(expression: Application): Pattern = expression.toPattern()

    def λ(variable: Pattern)(term: Expression) = Lambda(variable, term)

    def num(number: Int) = Number(number)

    import scala.language.dynamics
    object let extends Dynamic:
      def applyDynamicNamed(kind: String)(args: (String, Expression)*) =
        if kind == "rec" then
          val defs = for (name, value) <- args if name != "" yield Definition(Variable(name), value)
          val in = args.find(_._1 == "")
          LetRec(defs.toList, in.get._2)
        else
          null
