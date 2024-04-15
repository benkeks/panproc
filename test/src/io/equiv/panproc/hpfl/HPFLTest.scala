package hpfltest

import io.equiv.panproc.hpfl.*
import io.equiv.panproc.hpfl.HPFLCore.*
import io.equiv.panproc.ts.TransitionSystem
import io.equiv.panproc.relations.LabeledRelation
import munit.*

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
    println(evalResult)
  }

  test("check default trace equivalence") {
    val lts1 =
      TransitionSystem(LabeledRelation((0, 'b', 1), (1, 'a', 1), (1, 'b', 0)), Map(0 -> 0, 1 -> 1))
    val lts2 = TransitionSystem(
      LabeledRelation((2, 'b', 3), (3, 'a', 4), (3, 'b', 2), (4, 'a', 3), (4, 'b', 2)),
      Map(2 -> 2, 3 -> 3, 4 -> 4)
    )
    val result = DefaultEquivalences.trace.check(List(lts1, lts2))
    println(result)
  }
