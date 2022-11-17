package io.equiv.panproc.ccs


object Syntax:

  abstract sealed class Expression()

  case class ProcessDeclaration(name: String, process: ProcessExpression) extends Expression():
    override def toString() = name + " = " + process.toString()

  case class Label(name: String) extends Expression():
    override def toString() = name
    def isOutput = name.endsWith("!")
    def toOutput = if (isOutput) this else Label(name + "!")
    def toInput: Label = if (isOutput) Label(name.dropRight(1)).toInput else this

  abstract sealed class ProcessExpression() extends Expression():

    def asContext(insertion: ProcessExpression): ProcessExpression

    infix def +(other: ProcessExpression) =
      Choice(List(this, other))

    infix def |(other: ProcessExpression) =
      Parallel(List(this, other))

    infix def *:(name: String) =
      Prefix(Label(name), this)

  case class Prefix(val l: Label, val proc: ProcessExpression)
    extends ProcessExpression():
    
    override def toString() =
      val ps = proc.toString()
      if l.isOutput then
        l.toString + (if ps.contains(" ") then "(" + ps + ")" else ps)
      else
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

  case class Restrict(val names: List[Label], val proc: ProcessExpression) extends ProcessExpression():

    override def toString() =
      val ps = proc.toString()
      (if ps.contains(" ") then "(" + ps + ")" else ps) + names.mkString(" \\ {",",","}")

    override def asContext(insertion: ProcessExpression): ProcessExpression =
      Restrict(names, insertion)

  case class ProcessName(val l: Label) extends ProcessExpression():
    override def toString() = l.toString
    override def asContext(insertion: ProcessExpression): ProcessExpression = this

  case class MetaDeclaration(key: String, value: String) extends Expression()
  
  case class Definition(val defs: List[Expression]) extends Expression():

    val metaInfo = defs.collect { case md: MetaDeclaration => md }.groupBy(_.key)
    
    def getDeclaration(processID: String): Option[ProcessDeclaration] =
      defs collectFirst {
        case pd @ ProcessDeclaration(n, _) if n == processID => pd
      }