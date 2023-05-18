package io.equiv.panproc.ccs

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Expression
import io.equiv.panproc.lambda.Syntax.Intermediate
import io.equiv.panproc.lambda.Syntax.Literal

object Syntax:

  case class Name(name: String):
    override def toString() = name

  object Label:
    def apply(name: String): Label = apply(Name(name), None)

  case class Label(name: Name, argument: Option[Expression] = None) extends Literal():

    override def pretty = argument match
      case None        => name.name
      case Some(value) => s"${name.name}(${value.pretty})"
    override def toString(): String = name.name

  sealed trait ProcessExpression() extends Intermediate:

    infix def +(other: Expression) =
      Choice(List(this, other))

    infix def |(other: Expression) =
      Parallel(List(this, other))

    infix def \(restrictedNames: Iterable[String]) =
      Restrict(restrictedNames.toList.map(Name(_)), this)

  case class Send(val l: Label, val proc: Expression) extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      l.pretty + "!." + (if ps.contains(" ") then "(" + ps + ")" else ps)

  case class Receive(val l: Label, val proc: Expression) extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      l.pretty + "." + (if ps.contains(" ") then "(" + ps + ")" else ps)


  case class Choice(val procs: List[Expression]) extends ProcessExpression():

    override def pretty =
      if procs.isEmpty then
        "0"
      else
        val str = procs.map(_.pretty).mkString(" + ")
        if str.contains("|") then "(" + str + ")" else str


  case class Parallel(val procs: List[Expression]) extends ProcessExpression():

    override def pretty =
      if procs.isEmpty then
        "0"
      else
        procs.map(_.pretty).mkString(" | ")


  case class Restrict(val names: List[Name], val proc: Expression)
      extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" ⧹ {", ",", "}")


  object Notation:

    val nullProcess = Choice(Nil)

    def send(channelName: String) = SendBuilder(channelName, None)
    def send(channelName: String, argument: Expression) = SendBuilder(channelName, Some(argument))
    final class SendBuilder(channelName: String, argument: Option[Expression])
        extends Send(Label(Name(channelName), argument), nullProcess):
      infix def *(continuation: Expression) =
        Send(Label(Name(channelName), argument), continuation)

    def receive(channelName: String) = ReceiveBuilder(channelName, None)
    def receive(channelName: String, matcher: lambda.Syntax.Pattern) =
      ReceiveBuilder(channelName, Some(matcher))
    final class ReceiveBuilder(channelName: String, matcher: Option[lambda.Syntax.Pattern])
        extends Receive(Label(Name(channelName)), nullProcess):
      infix def *(continuation: Expression) = matcher match
        case Some(matcher) =>
          Receive(Label(Name(channelName)), lambda.Syntax.Lambda(matcher, continuation))
        case None =>
          Receive(Label(Name(channelName)), continuation)

    def subProcess(
        processName: String,
        argument: Expression = lambda.Syntax.Unit()
    ): ProcessExpression =
      Choice(List(lambda.Syntax.Application(lambda.Syntax.Variable(processName), argument)))
