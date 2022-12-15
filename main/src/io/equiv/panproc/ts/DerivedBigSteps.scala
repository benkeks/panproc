package io.equiv.panproc.ts

trait DerivedBigSteps[E, Env, S, A, L] {
  self: AbstractOperationalSemantics[E, Env, S, A, L] =>

  def isValue(e: E): Boolean

  def eval(expr: E): Iterable[E] =
    val proc0 = stateIds(expr)
    val trans0 = for
      (a, expr1) <- localSemantics(procEnv)(expr).toList
    yield (a, stateIds(expr1), expr1)
    if !transitions.isDefinedAt(proc0) then
      transitions(proc0) = for (a, s1, _) <- trans0 yield (a, s1)
    for
      (a, _, e1) <- trans0
      r <-
        if isValue(e1) then
          List(e1)
        else
          eval(e1)
    yield
      r
}
