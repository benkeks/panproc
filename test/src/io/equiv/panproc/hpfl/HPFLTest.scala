package hpfltest

import io.equiv.panproc.hpfl.*
import io.equiv.panproc.hpfl.HPFLCore.*
import io.equiv.panproc.ts.TransitionSystem
import io.equiv.panproc.relations.LabeledRelation
import munit.*

object TestData:
  def makeTS[E, L](tuples: (E, L, E)*): TransitionSystem[E, L, E] =
    val allStates = tuples.flatMap(t => List(t._1, t._3)).toSet
    TransitionSystem(LabeledRelation(tuples*), allStates.map(s => s -> s).toMap)

  val t1: List[TransitionSystem[Int, Char, Int]] =
    List(
      makeTS((1, 'a', 2), (2, 'b', 3)),
      makeTS((1, 'a', 2), (2, 'b', 3), (1, 'a', 4))
    )
  val t1_success: Option[Set[List[Int]]] = Some(Set(
    List(1, 1),
    List(2, 2),
    List(3, 3),
    List(3, 4)
  ))
  val t1_failure: Option[Set[List[Int]]] = Some(Set(
    List(2, 2),
    List(3, 3),
    List(3, 4)
  ))

  val t2: List[TransitionSystem[Int, Char, Int]] =
    List(
      makeTS((1, 'a', 2), (2, 'c', 3), (1, 'a', 4), (4, 'b', 5)),
      makeTS((1, 'a', 2), (2, 'c', 3), (2, 'b', 4))
    )
  val t2_success: Option[Set[List[Int]]] = Some(Set(
    List(1, 1),
    List(3, 3),
    List(3, 4),
    List(5, 3),
    List(5, 4)
  ))
  val t2_failure: Option[Set[List[Int]]] = Some(Set(
    List(3, 3),
    List(3, 4),
    List(5, 3),
    List(5, 4)
  ))

  val t3: List[TransitionSystem[Int, Char, Int]] =
    List(
      makeTS(
        (1, 'a', 2),
        (1, 'a', 5),
        (2, 'b', 3),
        (2, 'c', 4),
        (3, 'b', 4),
        (5, 'a', 4),
        (5, 'b', 6),
        (6, 'a', 4)
      ),
      makeTS(
        (1, 'a', 2),
        (1, 'a', 5),
        (2, 'b', 3),
        (2, 'c', 4),
        (3, 'a', 4),
        (5, 'a', 4),
        (5, 'b', 6),
        (6, 'b', 4)
      )
    )
  val t3_success: Option[Set[List[Int]]] = Some(Set(
    List(1, 1),
    List(3, 6),
    List(4, 4),
    List(6, 3)
  ))
  val t3_failure: Option[Set[List[Int]]] = Some(Set(
    List(3, 6),
    List(4, 4),
    List(6, 3)
  ))

  val t4: List[TransitionSystem[Int, Char, Int]] = List(
    makeTS(
      (1, 'a', 2),
      (1, 'a', 4),
      (2, 'c', 3),
      (4, 'b', 5)
    ),
    makeTS(
      (1, 'a', 2),
      (1, 'a', 4),
      (1, 'a', 5),
      (2, 'c', 3),
      (4, 'b', 6),
      (4, 'c', 3),
      (5, 'b', 6)
    )
  )
  val t4_success: Option[Set[List[Int]]] =
    Some(Set(
      List(1, 1),
      List(2, 2),
      List(3, 3),
      List(3, 6),
      List(4, 5),
      List(5, 3),
      List(5, 6)
    ))
  val t4_failure: Option[Set[List[Int]]] =
    Some(Set(
      List(2, 2),
      List(3, 3),
      List(3, 6),
      List(4, 5),
      List(5, 3),
      List(5, 6)
    ))

  val t5: List[TransitionSystem[Int, Char, Int]] = List(
    makeTS(
      (1, 'a', 2),
      (1, 'a', 5),
      (2, 'b', 3),
      (3, 'b', 4),
      (5, 'b', 6),
      (6, 'a', 4)
    ),
    makeTS(
      (1, 'a', 2),
      (2, 'b', 3),
      (3, 'b', 4),
      (2, 'b', 5),
      (5, 'a', 4)
    )
  )
  val t5_success: Option[Set[List[Int]]] = Some(Set(
    List(1, 1),
    List(3, 3),
    List(4, 4),
    List(6, 5)
  ))
  val t5_failure: Option[Set[List[Int]]] = Some(Set(
    List(3, 3),
    List(4, 4),
    List(6, 5)
  ))

  val t6: List[TransitionSystem[Int, Char, Int]] = List(
    makeTS(
      (1, 'a', 2),
      (1, 'a', 5),
      (2, 'b', 3),
      (2, 'b', 4),
      (4, 'a', 4),
      (5, 'b', 4)
    ),
    makeTS(
      (1, 'a', 2),
      (2, 'b', 3),
      (2, 'b', 4),
      (4, 'a', 4)
    )
  )
  val t6_success_1: Option[Set[List[Int]]] = Some(Set(
    List(1, 1),
    List(2, 2),
    List(3, 3),
    List(4, 4),
    List(5, 2)
  ))
  val t6_success_2: Option[Set[List[Int]]] = Some(Set(
    List(1, 1),
    List(2, 2),
    List(3, 3),
    List(4, 4)
  ))

  val t6_failure: Option[Set[List[Int]]] = Some(Set(
    List(2, 2),
    List(3, 3),
    List(4, 4)
  ))

class HPFLSuite extends munit.FunSuite:
  test("typecheck succeeds") {
    val formula: HPFLCore[Char, Char, Unit] = app(
      mu(
        'F',
        lam(
          'X',
          obs(
            'a',
            1,
            and(Set(variable('Y'), app(variable('F'), neg(app(variable('F'), variable('X'))))))
          )
        )
      ),
      neg(
        obs('b', 1, neg(variable('Y')))
      )
    )
    val env = emptyTypeEnvironment[Char].updated('Y', (Variance.Any, Ground))
    val result = typecheck(formula, Ground, env)
    assert(result.isDefined)
  }

  test("typecheck fails") {
    val formula = mu('X', neg(variable('X')))
    val result = typecheck(formula)
    assert(result.isEmpty)
  }

  test("typecheck trace equivalence") {
    val formula =
      app(
        app(
          nu(
            'F',
            lam(
              'X',
              lam(
                'Y',
                and(Set(
                  iff(variable('X'), variable('Y')),
                  app(app(variable('F'), obs('a', 1, variable('X'))), obs('a', 2, variable('Y')))
                ))
              )
            )
          ),
          top
        ),
        top
      )
    val result = typecheck(formula)
    assert(result.isDefined)
  }

  test("eval trace equivalence") {
    val formula =
      app(
        app(
          nu(
            'F',
            lam(
              'X',
              lam(
                'Y',
                and(Set(
                  iff(variable('X'), variable('Y')),
                  app(app(variable('F'), obs('a', 1, variable('X'))), obs('a', 2, variable('Y'))),
                  app(app(variable('F'), obs('b', 1, variable('X'))), obs('b', 2, variable('Y')))
                ))
              )
            )
          ),
          top
        ),
        top
      )
    val result = typecheck(formula)
    val lts1 =
      TransitionSystem(LabeledRelation((0, 'b', 1), (1, 'a', 1), (1, 'b', 0)), Map(0 -> 0, 1 -> 1))
    val lts2 = TransitionSystem(
      LabeledRelation((2, 'b', 3), (3, 'a', 4), (3, 'b', 2), (4, 'a', 3), (4, 'b', 2)),
      Map(2 -> 2, 3 -> 3, 4 -> 4)
    )
    val evalResult = eval(result.get, List(), Map(), Map(), List(lts1, lts2))
    assert(evalResult == Ok(Set(List(1, 3), List(1, 4), List(0, 2))))
  }

  test("eval simulation") {
    def helper(c: Char) = nec(c, 1, obs(c, 2, variable('X')))
    val formula =
      nu('X', and(Set(helper('a'), helper('b'))))
    val result = typecheck(formula)
    val lts1 =
      TransitionSystem(LabeledRelation((0, 'b', 1), (1, 'a', 1), (1, 'b', 0)), Map(0 -> 0, 1 -> 1))
    val lts2 = TransitionSystem(
      LabeledRelation((2, 'b', 3), (3, 'a', 4), (3, 'b', 2), (4, 'a', 3), (4, 'b', 2)),
      Map(2 -> 2, 3 -> 3, 4 -> 4)
    )
    val evalResult = eval(result.get, List(), Map(), Map(), List(lts1, lts2))
    assert(evalResult == Ok(Set(List(1, 3), List(1, 4), List(0, 2))))
  }

  test("check default trace equivalence") {
    assert(DefaultEquivalences.trace.check(TestData.t1) == TestData.t1_success)
    assert(DefaultEquivalences.trace.check(TestData.t2) == TestData.t2_success)
    assert(DefaultEquivalences.trace.check(TestData.t3) == TestData.t3_success)
    assert(DefaultEquivalences.trace.check(TestData.t4) == TestData.t4_success)
    assert(DefaultEquivalences.trace.check(TestData.t5) == TestData.t5_success)
    assert(DefaultEquivalences.trace.check(TestData.t6) == TestData.t6_success_1)
  }

  test("check completed trace equivalence") {
    assert(DefaultEquivalences.completedTrace.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.completedTrace.check(TestData.t2) == TestData.t2_success)
    assert(DefaultEquivalences.completedTrace.check(TestData.t3) == TestData.t3_success)
    assert(DefaultEquivalences.completedTrace.check(TestData.t4) == TestData.t4_success)
    assert(DefaultEquivalences.completedTrace.check(TestData.t5) == TestData.t5_success)
    assert(DefaultEquivalences.completedTrace.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check failures equivalence") {
    assert(DefaultEquivalences.failures.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.failures.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.failures.check(TestData.t3) == TestData.t3_success)
    assert(DefaultEquivalences.failures.check(TestData.t4) == TestData.t4_success)
    assert(DefaultEquivalences.failures.check(TestData.t5) == TestData.t5_success)
    assert(DefaultEquivalences.failures.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check failure trace equivalence") {
    assert(DefaultEquivalences.failureTrace.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.failureTrace.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.failureTrace.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.failureTrace.check(TestData.t4) == TestData.t4_success)
    assert(DefaultEquivalences.failureTrace.check(TestData.t5) == TestData.t5_success)
    assert(DefaultEquivalences.failureTrace.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check readiness equivalence") {
    assert(DefaultEquivalences.readiness.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.readiness.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.readiness.check(TestData.t3) == TestData.t3_success)
    assert(DefaultEquivalences.readiness.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.readiness.check(TestData.t5) == TestData.t5_success)
    assert(DefaultEquivalences.readiness.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check ready trace equivalence") {
    assert(DefaultEquivalences.readyTrace.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.readyTrace.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.readyTrace.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.readyTrace.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.readyTrace.check(TestData.t5) == TestData.t5_success)
    assert(DefaultEquivalences.readyTrace.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check simulation equivalence") {
    assert(DefaultEquivalences.simulation.check(TestData.t1) == TestData.t1_success)
    assert(DefaultEquivalences.simulation.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.simulation.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.simulation.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.simulation.check(TestData.t5) == TestData.t5_failure)
    assert(DefaultEquivalences.simulation.check(TestData.t6) == TestData.t6_success_1)
  }

  test("check completed simulation equivalence") {
    assert(DefaultEquivalences.completedSimulation.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.completedSimulation.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.completedSimulation.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.completedSimulation.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.completedSimulation.check(TestData.t5) == TestData.t5_failure)
    assert(DefaultEquivalences.completedSimulation.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check ready simulation equivalence") {
    assert(DefaultEquivalences.readySimulation.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.readySimulation.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.readySimulation.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.readySimulation.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.readySimulation.check(TestData.t5) == TestData.t5_failure)
    assert(DefaultEquivalences.readySimulation.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check 2-nested simulation equivalence") {
    assert(DefaultEquivalences.twoNestedSimulation.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.twoNestedSimulation.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.twoNestedSimulation.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.twoNestedSimulation.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.twoNestedSimulation.check(TestData.t5) == TestData.t5_failure)
    assert(DefaultEquivalences.twoNestedSimulation.check(TestData.t6) == TestData.t6_success_2)
  }

  test("check bisimulation equivalence") {
    assert(DefaultEquivalences.bisimulation.check(TestData.t1) == TestData.t1_failure)
    assert(DefaultEquivalences.bisimulation.check(TestData.t2) == TestData.t2_failure)
    assert(DefaultEquivalences.bisimulation.check(TestData.t3) == TestData.t3_failure)
    assert(DefaultEquivalences.bisimulation.check(TestData.t4) == TestData.t4_failure)
    assert(DefaultEquivalences.bisimulation.check(TestData.t5) == TestData.t5_failure)
    assert(DefaultEquivalences.bisimulation.check(TestData.t6) == TestData.t6_failure)
  }
