package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.Notation._
import io.equiv.panproc.ccs.Semantics

import io.equiv.panproc.lambda
import io.equiv.panproc.lambda.Syntax.Notation.{_, given}

@main def panproc() =

  val lambdaProg = let.rec(
    a = λ("x")("b"("x")),
    b = λ("x")("x"),
    "a"("a")
  )

  val constructorTest = let.rec(
    myObject = "Cons"(1)(2),
    myProjection = λ("Cons"("x")("y"))("y"),
    "myProjection"("myObject")
  )

  val ccsProg = let.rec(
    P1 =  λ("x")(send("hello") * subProcess("P2") + receive("stop") * nullProcess),
    P2 =  λ("x")(send("reload") * (receive("hello") * subProcess("P1"))),
    (subProcess("P1") | subProcess("P2")) \ Set("hello")
  )

  val ccsIterProg = let.rec(
    P1 = λ("x")(receive("world") * subProcess("P1")),
    subProcess("P1")
  )
  //println(lambda.CallByValueBigStepSemantics(ccsIterProg).asTransitionSystem())
  println(Semantics(constructorTest).semantics())

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
