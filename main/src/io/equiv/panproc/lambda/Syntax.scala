package io.equiv.panproc.lambda

object Syntax:

  abstract sealed class Expression:
    def apply(other: Expression) = Application(this, other)
    def pretty: String

  abstract class Pattern:
    def pretty: String

  case class Name(name: String) extends Pattern:
    override def toString() = name
    def pretty: String = name

  case class Lambda(variable: Name, term: Expression) extends Expression:
    override def pretty = s"(λ${variable.pretty}. ${term.pretty})"

  case class Variable(variable: Name) extends Expression:
    override def pretty = variable.pretty

  case class Application(function: Expression, argument: Expression) extends Expression:
    override def pretty = s"${function.pretty} ${argument.pretty}"

  case class Definition(pattern: Pattern, value: Expression):
    def pretty = s"${pattern.pretty} = ${value.pretty}"

  case class LetRec(definitions: List[Definition], in: Expression) extends Expression:
    override def pretty = s"letrec ${definitions.map(_.pretty).mkString("; ")} in ${in.pretty}"

  abstract class Literal extends Expression
  abstract class Intermediate extends Expression

  object Notation:

    def λ(variable: String)(term: Expression) = Lambda(Name(variable), term)

    def atom(name: String) = Variable(Name(name))

    import scala.language.dynamics
    object let extends Dynamic:
      def applyDynamicNamed(kind: String)(args: (String, Expression)*) =
        if kind == "rec" then
          val defs = for (name, value) <- args if name != "" yield Definition(Name(name), value)
          val in = args.find(_._1 == "")
          LetRec(defs.toList, in.get._2)
        else
          null
