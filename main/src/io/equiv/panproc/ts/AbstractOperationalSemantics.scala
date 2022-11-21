package io.equiv.panproc.ts

import io.equiv.panproc.relations.LabeledRelation

trait AbstractOperationalSemantics[E, Env, S, A, L](mainExpr: E):

  def stateIds(expr: E): S

  def globalEnvironment(mainExpr: E): (Env, Iterable[E])

  def localSemantics(procEnv: Env)(e: E): Iterable[(A, E)]

  val transitions = collection.mutable.Map[S, List[(A, S)]]()
  private val todo = collection.mutable.Buffer[E]()

  def asTransitionSystem() =
    val (steps, nodes) = semantics()
    TransitionSystem(steps, nodes)

  def semantics(): (LabeledRelation[S, A], Map[S, L]) =
    val (procEnv, initialProcs) = globalEnvironment(mainExpr)

    initialProcs.foreach(scheduleConversion)

    while todo.nonEmpty do
      val proc0 = todo.remove(0)
      val state0 = stateIds(proc0)
      if !transitions.isDefinedAt(state0) then
        transitions(state0) =
          for
            (a, proc1) <- localSemantics(procEnv)(proc0).toList
          yield (a, scheduleConversion(proc1))

    val interestingNodes =
      initialProcs

    val trans =
      for
        (s0, as1) <- transitions.toSet
        (a, s1) <- as1
      yield (s0, a, s1)

    // val labelMap = nodeDecls.toMap
    val relation = new LabeledRelation(trans).filterReachable(interestingNodes.map(stateIds(_)))

    (relation, Map())

  def scheduleConversion(e: E): S =
    val stateId = stateIds(e)
    if !transitions.isDefinedAt(stateId) then
      todo += e
    stateId
