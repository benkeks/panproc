package io.equiv.panproc.tool

import io.equiv.panproc.ccs.Syntax
import io.equiv.panproc.ccs.Syntax.{ NullProcess, RecProc }
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

  val ccsProg = let.rec(
    P1 =  λ("x")("hello!" *: RecProc("P2") + "stop" *: NullProcess()),
    P2 =  λ("x")("reload!" *: "hello" *: RecProc("P1")),
    (RecProc("P1") | RecProc("P2")) \ Set("hello")
  )

  val ccsIterProg = let.rec(
    P1 = λ("x")("world" *: RecProc("P1")),
    atom("P1")(num(0))
  )
  //println(lambda.CallByValueBigStepSemantics(ccsIterProg).asTransitionSystem())
  println(Semantics(ccsProg).semantics())

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
