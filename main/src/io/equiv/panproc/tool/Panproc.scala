package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.NullProcess

@main def panproc() =
  println("Hello")

  println(
    "w" *: ("x" *: "y" *: "z" *: NullProcess() + "y" *: NullProcess()) | "x" *: NullProcess()
  )