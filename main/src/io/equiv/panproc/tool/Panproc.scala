package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.NullProcess
import io.equiv.panproc.ccs.Semantics

@main def panproc() =
  println("Hello")

  val proc = "w" *: ("x" *: "y" *: "z" *: Syntax.ProcessName(
    Syntax.Name("A")
  ) + "y" *: NullProcess()) // + "x" *: NullProcess()
  println(
    proc
  )

  val defs = Syntax.Definition(
    List(Syntax.ProcessDeclaration("A", proc)),
    List(Syntax.ProcessName(Syntax.Name("A")))
  )

  println(Semantics().semantics(defs))
