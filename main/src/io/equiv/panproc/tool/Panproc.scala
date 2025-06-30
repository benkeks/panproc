package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.Notation._
import io.equiv.panproc.ccs.Semantics

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Notation.{_, given}

@main def panproc() =

  val lambdaProg =
    λ("x")("x"("x"))(λ("y")("b"("y"))("z"))
  println(lambda.CallByNameSimpSemantics(lambdaProg).asTransitionSystem().toMermaid())
  println(lambda.CallByValueSimpSemantics(lambdaProg).asTransitionSystem().toMermaid())
  //println(Semantics(ccsProg).semantics())



  // val letRecProg = let.rec(
  //   a = λ("x")("b"("x")),
  //   b = λ("x")("x"),
  //   "a"("a")
  // )

  //println(lambda.CallByValueSemantics(lambdaProg).asTransitionSystem().toMermaid(prettyPrint = _.pretty))


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
