package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.NullProcess
import io.equiv.panproc.ccs.Semantics

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Notation._


@main def panproc() =
  println("Hello")

  val lambdaProg = let.rec(
    a = λ("x")(atom("b")(atom("x"))),
    b = λ("x")(atom("x")),
    atom("a")(atom("a"))
  )

  //println(lambdaProg)

  println(lambda.CallByValueSemantics(lambdaProg).asTransitionSystem().toMermaid(prettyPrint = _.pretty))


  // val proc = "w" *: ("x" *: "y" *: "z" *: Syntax.ProcessName(
  //   Syntax.Name("A")
  // ) + "y" *: NullProcess()) // + "x" *: NullProcess()
  // println(
  //   proc
  // )

  // val defs = Syntax.Definition(
  //   List(Syntax.ProcessDeclaration("A", proc)),
  //   List(Syntax.ProcessName(Syntax.Name("A")))
  // )

  // println(Semantics(defs).asTransitionSystem().toMermaid())
