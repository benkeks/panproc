package io.equiv.panproc.ccs

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.*

object Syntax:

  sealed trait ProcessExpression() extends Intermediate:

    infix def +(other: Expression) =
      Choice(List(this, other))

    infix def |(other: Expression) =
      Parallel(List(this, other))

    infix def \(restrictedNames: Iterable[Pattern]) =
      Restrict(restrictedNames.toList, this)

  case class Send(val emitted: Expression, val continuation: Expression) extends ProcessExpression():

    override def pretty =
      val ps = continuation.pretty
      emitted.pretty + "!." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    infix def *(continuation: Expression) =
        Send(emitted, continuation)

  case class Receive(val receiver: Lambda) extends ProcessExpression():

    override def pretty =
      val ps = receiver.term.pretty
      receiver.variable.pretty + "." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    infix def *(continuation: Expression) =
        Receive(Lambda(receiver.variable, continuation))

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


  case class Restrict(val names: List[Pattern], val proc: Expression)
      extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" ⧹ {", ",", "}")


  object Notation:

    val nullProcess = Choice(Nil)

    def send(argument: Expression) =
      Send(argument, nullProcess)

    def receive(matcher: Pattern) =
      Receive(Lambda(matcher, nullProcess))

    def subProcess(
        processName: String,
        argument: Expression = lambda.Syntax.Unit()
    ): ProcessExpression =
      Choice(List(Application(Variable(processName), argument)))
