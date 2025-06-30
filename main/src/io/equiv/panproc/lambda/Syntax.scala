package io.equiv.panproc.lambda

object Syntax:

  abstract sealed class Expression:
    def apply(other: Expression) = Application(this, other)
    def pretty: String
    def prettyTex: String
    def freeVariables: Set[String]
    def isClosed() = (freeVariables.isEmpty)
    def substituteAll(fillIns: Map[String, Expression]): Expression

  trait DefaultPrettyPrinting(boxed: Any):
    def pretty: String = boxed.toString()
    def prettyTex: String = boxed.toString()

  trait Pattern:
    def pretty: String
    def prettyTex: String
    def freeVariables: Set[String]

  case class Variable(name: String) extends Expression, Pattern:
    override def toString() = name
    def pretty: String = name
    def prettyTex: String = s"\\mathit{$name}"
    def freeVariables = Set(name)
    def substituteAll(fillIns: Map[String, Expression]): Expression = fillIns.getOrElse(name, this)

  case class Constructor(left: Pattern, right: Pattern) extends Pattern:
    override def pretty: String = s"(${left.pretty} ${right.pretty})"
    override def prettyTex: String = s"(${left.prettyTex} ${right.prettyTex})"
    override def freeVariables = left.freeVariables ++ right.freeVariables

  case class Lambda(val variable: Pattern, val term: Expression) extends Expression:
    override def pretty = s"(λ${variable.pretty}. ${term.pretty})"
    override def prettyTex = s"(\\lambda ${variable.prettyTex} \\ldotp  ${term.prettyTex})"
    override def freeVariables = term.freeVariables -- variable.freeVariables
    def substituteAll(fillIns: Map[String, Expression]): Lambda = Lambda(
      variable match
        case Variable(name) if fillIns.isDefinedAt(name) => 
          throw Exception(s"Variable substitution failed for ${variable.pretty}")
        case _ => variable,
      term.substituteAll(fillIns)
    )

  case class Application(function: Expression, argument: Expression) extends Expression:

    override def pretty =
      if (!argument.isInstanceOf[Lambda] && !argument.isInstanceOf[Variable]) then
        s"${function.pretty} (${argument.pretty})"
      else
        s"${function.pretty} ${argument.pretty}"
    
    override def prettyTex = s"${function.prettyTex} \\left( ${argument.prettyTex} \\right)"

    override def freeVariables = function.freeVariables ++ argument.freeVariables

    def substituteAll(fillIns: Map[String, Expression]): Application = Application(
      function.substituteAll(fillIns),
      argument.substituteAll(fillIns)
    )

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
    def boundVariables = pattern.freeVariables
    def freeVariables = value.freeVariables -- boundVariables

  case class LetRec(definitions: List[Definition], in: Expression) extends Expression:
    override def pretty = s"letrec ${definitions.map(_.pretty).mkString("; ")} in ${in.pretty}"
    override def prettyTex = s"\\mathsf{let rec}\\\\\\quad ${definitions.map(_.prettyTex).mkString(" \\\\\\quad ")} \\\\ \\mathsf{in}\\; ${in.prettyTex}"
    override def freeVariables = in.freeVariables ++ definitions.flatMap(_.freeVariables) -- definitions.flatMap(_.boundVariables)
    def substituteAll(fillIns: Map[String, Expression]): Expression =
      LetRec(
        definitions.map { case Definition(pattern, value) => Definition(pattern, value.substituteAll(fillIns)) },
        in.substituteAll(fillIns)
      )

  abstract class Literal extends Expression, Pattern:
    override def freeVariables: Set[String] = Set()
    override def substituteAll(fillIns: Map[String, Expression]): Expression = this

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
