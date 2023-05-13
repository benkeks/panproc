package io.equiv.panproc.ts

trait DerivedBigSteps[E, Env, A, L]:
  self: AbstractOperationalSemantics[E, Env, E, A, L] =>

  def isValue(e: E): Boolean

  def eval(sem: E => Iterable[(A, E)], stopAt: E => Boolean)(expr: E): Iterable[E] =
    val proc0 = stateIds(expr)
    println(proc0)
    if isValue(proc0) || stopAt(proc0) then
      List(proc0)
    else
      for
        (a, expr1) <- sem(expr)
        r <- eval(sem, stopAt)(stateIds(expr1))
      yield
        r
