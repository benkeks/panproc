package hpfltest

import io.equiv.panproc.hpfl.*
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
