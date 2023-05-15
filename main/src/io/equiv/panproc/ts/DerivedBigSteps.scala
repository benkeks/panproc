package io.equiv.panproc.ts

trait DerivedBigSteps[E, Env, A, L]:
  self: AbstractOperationalSemantics[E, Env, E, A, L] =>

  def isValue(e: E): Boolean

  def eval(sem: E => Iterable[(A, E)], stopAt: E => Boolean)(expr: E): Iterable[E] =
    val proc0 = stateIds(expr)
    println("eval: " + proc0)
    if isValue(proc0) || stopAt(proc0) then
      println("eval result: " + proc0)
      List(proc0)
    else
      val continuations = sem(expr)
      if continuations.isEmpty then
        println("eval stuck result: " + proc0)
        List(proc0)
      else
        for
          (a, expr1) <- continuations
          r <- eval(sem, stopAt)(stateIds(expr1))
        yield
          r

  def evalDelay(sem: E => Iterable[(A, E)], internal: A)(expr: E): Iterable[(A,E)] =
    val proc0 = stateIds(expr)
    println("eval internal: " + proc0)
    if isValue(proc0) then
      println("eval result: " + proc0)
      List(internal -> proc0)
    else
      val continuations = sem(expr)
      if continuations.isEmpty then
        println("eval stuck result: " + proc0)
        List(internal -> proc0)
      else
        for
          (a, expr1) <- continuations
          (ar, er) <-
            if a == internal then
              evalDelay(sem, internal)(stateIds(expr1))
            else
              List(a -> expr1)
        yield
          (ar -> er)