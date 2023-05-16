package io.equiv.panproc.ccs

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.lambda.Syntax.Intermediate
import io.equiv.panproc.lambda.Syntax.Literal

object Syntax:

  case class Name(name: String):
    override def toString() = name

  case class Label(name: Name, argument: Option[Expression] = None) extends Literal():
    override def pretty = argument match
      case None        => name.name
      case Some(value) => s"${name.name}($value)"
    override def toString(): String = name.name

  abstract sealed class ProcessExpression() extends Intermediate():

    def asContext(insertion: Expression): ProcessExpression

    infix def +(other: Expression) =
      Choice(List(this, other))

    infix def |(other: Expression) =
      Parallel(List(this, other))

    private val receivePattern = raw"(\w+)\((\w+)\)".r
    private val sendPattern = raw"(\w+)\!\((\w+)\)".r

    infix def *:(name: String) =
      if (name.contains("!")) then
        name match
          case sendPattern(channel, variable) =>
            Send(
              Label(Name(channel), Some(lambda.Syntax.Variable(lambda.Syntax.Name(variable)))),
              this
            )
          case _ if name.endsWith("!") =>
            Send(Label(Name(name.dropRight(1))), this)
          case _ => throw new Exception("Wrong send pattern.")
      else
        name match
          case receivePattern(channel, variable) =>
            Receive(Label(Name(channel)), lambda.Syntax.Lambda(lambda.Syntax.Name(variable), this))
          case _ =>
            Receive(Label(Name(name)), this)

    infix def \(restrictedNames: Iterable[String]) =
      Restrict(restrictedNames.toList.map(Name(_)), this)

  case class Send(val l: Label, val proc: Expression) extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      l.pretty + "!." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    override def asContext(insertion: Expression): ProcessExpression =
      Send(l, insertion)

  case class Receive(val l: Label, val proc: Expression) extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      l.pretty + "." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    override def asContext(insertion: Expression): ProcessExpression =
      Receive(l, insertion)

  case class Choice(val procs: List[Expression]) extends ProcessExpression():

    override def pretty =
      if procs.isEmpty then
        "0"
      else
        val str = procs.map(_.pretty).mkString(" + ")
        if str.contains("|") then "(" + str + ")" else str

    override def asContext(insertion: Expression): ProcessExpression =
      Choice(procs :+ insertion)

  def NullProcess() = Choice(Nil)
  def RecProc(name: String) =
    Choice(List(
      lambda.Syntax.Application(
        lambda.Syntax.Variable(lambda.Syntax.Name(name)),
        lambda.Syntax.Number(0)
      )
    ))
  def RecProc(name: String, argument: Expression) =
    Choice(List(lambda.Syntax.Application(
      lambda.Syntax.Variable(lambda.Syntax.Name(name)),
      argument
    )))

  case class Parallel(val procs: List[Expression]) extends ProcessExpression():

    override def pretty =
      if procs.isEmpty then
        "0"
      else
        procs.map(_.pretty).mkString(" | ")

    override def asContext(insertion: Expression): ProcessExpression =
      Parallel(procs :+ insertion)

  case class Restrict(val names: List[Name], val proc: Expression)
      extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" ⧹ {", ",", "}")

    override def asContext(insertion: Expression): ProcessExpression =
      Restrict(names, insertion)
