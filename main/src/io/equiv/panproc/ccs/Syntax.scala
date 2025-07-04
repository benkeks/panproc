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

    override def freeVariables: Set[String] = emitted.freeVariables ++ continuation.freeVariables

    override def pretty =
      val ps = continuation.pretty
      emitted.pretty + "!." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    override def prettyTex =
      val ps = continuation.prettyTex
      s"\\overline{${emitted.prettyTex}} \\ldotp" + (if ps.contains(" ") then "(" + ps + ")" else ps)

    infix def *(newContinuation: Expression): Send =
      continuation match
      case Notation.nullProcess =>
        Send(emitted, newContinuation)
      case s @ Send(_, _) =>
        Send(emitted, s * newContinuation)
      case r @ Receive(_) =>
        Send(emitted, r * newContinuation)
      case _ =>
        throw Exception(s"Can't suffix a $continuation in CCS.")

  case class Receive(val receiver: Lambda) extends ProcessExpression():
    override def freeVariables: Set[String] = receiver.freeVariables

    override def pretty =
      val ps = receiver.term.pretty
      receiver.variable.pretty + "." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    override def prettyTex =
      val ps = receiver.term.prettyTex
      receiver.variable.prettyTex + "\\ldotp" + (if ps.contains(" ") then "(" + ps + ")" else ps)

    infix def *(newContinuation: Expression): Receive =
      receiver.term match
      case Notation.nullProcess =>
        Receive(Lambda(receiver.variable, newContinuation))
      case s @ Send(_, _) =>
        Receive(Lambda(receiver.variable, s * newContinuation))
      case r @ Receive(_) =>
        Receive(Lambda(receiver.variable, r * newContinuation))
      case _ =>
        throw Exception(s"Can't suffix a ${receiver.term} in CCS.")

  case class Choice(val procs: List[Expression]) extends ProcessExpression():

    override def pretty =
      if procs.isEmpty then
        "0"
      else
        val str = procs.map(_.pretty).mkString(" + ")
        if str.contains("|") then "(" + str + ")" else str

    override def prettyTex =
      if procs.isEmpty then
        "0"
      else
        val str = procs.map(_.prettyTex).mkString(" + ")
        if str.contains("\\mid") then "(" + str + ")" else str

    override def freeVariables: Set[String] = procs.flatMap(_.freeVariables).toSet


  case class Parallel(val procs: List[Expression]) extends ProcessExpression():

    override def pretty =
      if procs.isEmpty then
        "0"
      else
        procs.map(_.pretty).mkString(" | ")

    override def prettyTex =
      if procs.isEmpty then
        "0"
      else
        procs.map(_.prettyTex).mkString(" \\mid ")

    override def freeVariables: Set[String] = procs.flatMap(_.freeVariables).toSet


  case class Restrict(val names: List[Pattern], val proc: Expression)
      extends ProcessExpression():

    override def pretty =
      val ps = proc.pretty
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" ⧹ {", ",", "}")

    override def prettyTex =
      val ps = proc.prettyTex
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" \\setminus \\left\\{", ",", "\\right\\}")

    override def freeVariables: Set[String] = proc.freeVariables -- names.flatMap(_.freeVariables).toSet


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
