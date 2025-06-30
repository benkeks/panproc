package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.Notation._
import io.equiv.panproc.ccs.Semantics

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Notation.{_, given}

@main def panproc() =

  val lambdaProg =
    λ("y")("y")("z") 
    // λ("x")("y")(λ("z")("z")("q")) Ex from paper
    //    λ("x")("x"("x"))(λ("y")("b"("y"))("z"))

  println("Lambda Program:")
  println(lambdaProg.pretty)

  println("CBV:")
  println(lambda.CallByValueSimpSemantics(lambdaProg).asTransitionSystem().toMermaid())

  val cbpv1 = lambda.CallByPushValueSimpSemantics.encodeCallByValue(lambdaProg)
  println("CBPV for CBV:")
  println(cbpv1.pretty)
  println(lambda.CallByPushValueSimpSemantics(cbpv1).asTransitionSystem().toMermaid())

  // println(lambda.CallByNameSimpSemantics(lambdaProg).asTransitionSystem().toMermaid())



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
