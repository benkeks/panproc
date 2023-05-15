package io.equiv.panproc.ccs

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.lambda.Syntax.Intermediate
import io.equiv.panproc.lambda.Syntax.Literal

object Syntax:

  case class Name(name: String):
    override def toString() = name

  abstract class Label(name: Name) extends Literal():
    override def pretty = name.name

  case class Send(name: Name) extends Label(name):
    override def toString(): String = s"$name!⟨⟩"

  case class Receive(name: Name) extends Label(name):
    override def toString(): String = s"$name()"

  abstract sealed class ProcessExpression() extends Intermediate():

    def asContext(insertion: Expression): ProcessExpression

    infix def +(other: Expression) =
      Choice(List(this, other))

    infix def |(other: Expression) =
      Parallel(List(this, other))

    infix def *:(name: String) =
      if (name.endsWith("!")) then
        Prefix(Send(Name(name.dropRight(1))), this)
      else
        Prefix(Receive(Name(name)), this)

    infix def \(restrictedNames: Iterable[String]) =
      Restrict(restrictedNames.toList.map(Name(_)), this)

  case class Prefix(val l: Label, val proc: Expression) extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      l.pretty + "." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    override def asContext(insertion: Expression): ProcessExpression =
      println(insertion)
      Prefix(l, insertion)

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
        lambda.Syntax.Number(0))))
  def RecProc(name: String, argument: Expression) =
    Choice(List(lambda.Syntax.Application(lambda.Syntax.Variable(lambda.Syntax.Name(name)), argument)))

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
