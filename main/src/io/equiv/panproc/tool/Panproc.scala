package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax

@main def panproc() =
  println("Hello")

  println(Syntax.Build().receive("x").stop())