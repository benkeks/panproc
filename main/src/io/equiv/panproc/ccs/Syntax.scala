package io.equiv.panproc.ccs

object Syntax:

  abstract sealed class Expression()

  case class ProcessDeclaration(name: String, process: ProcessExpression) extends Expression():
    override def toString() = name + " = " + process.toString()

  case class Name(name: String):
    override def toString() = name

  abstract class Label(name: Name) extends Expression():
    override def toString() = name.name

  case class Send(name: Name) extends Label(name)

  case class Receive(name: Name) extends Label(name)

  case class Internal() extends Label(Name("tau"))

  abstract sealed class ProcessExpression() extends Expression():

    def asContext(insertion: ProcessExpression): ProcessExpression

    infix def +(other: ProcessExpression) =
      Choice(List(this, other))

    infix def |(other: ProcessExpression) =
      Parallel(List(this, other))

    infix def *:(name: String) =
      Prefix(Send(Name(name)), this)

  case class Prefix(val l: Label, val proc: ProcessExpression) extends ProcessExpression():

    override def toString() =
      val ps = proc.toString()
      l.toString + "." + (if ps.contains(" ") then "(" + ps + ")" else ps)

    override def asContext(insertion: ProcessExpression): ProcessExpression =
      println(insertion)
      Prefix(l, insertion)

  case class Choice(val procs: List[ProcessExpression]) extends ProcessExpression():

    override def toString() =
      if procs.isEmpty then
        "0"
      else
        val str = procs.mkString(" + ")
        if str.contains("|") then "(" + str + ")" else str

    override def asContext(insertion: ProcessExpression): ProcessExpression =
      Choice(procs :+ insertion)

  def NullProcess() = Choice(Nil)

  case class Parallel(val procs: List[ProcessExpression]) extends ProcessExpression():

    override def toString() =
      if procs.isEmpty then
        "0"
      else
        procs.mkString(" | ")

    override def asContext(insertion: ProcessExpression): ProcessExpression =
      Parallel(procs :+ insertion)

  case class Restrict(val names: List[Name], val proc: ProcessExpression)
      extends ProcessExpression():

    override def toString() =
      val ps = proc.toString()
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" \\ {", ",", "}")

    override def asContext(insertion: ProcessExpression): ProcessExpression =
      Restrict(names, insertion)

  case class ProcessName(val l: Name) extends ProcessExpression():
    override def toString()                                                 = l.toString
    override def asContext(insertion: ProcessExpression): ProcessExpression = this

  case class Definition(val defs: List[ProcessDeclaration], val mains: List[ProcessExpression])
      extends ProcessExpression():
    def getDeclaration(processID: String): Option[ProcessDeclaration] =
      defs collectFirst {
        case pd @ ProcessDeclaration(n, _) if n == processID => pd
      }

    def asContext(insertion: ProcessExpression): ProcessExpression =
      Definition(defs, mains :+ insertion)
