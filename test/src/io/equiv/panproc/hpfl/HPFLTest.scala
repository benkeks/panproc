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

  test("typecheck trace equivalence") {
    def or(a: HPFLCore[Char, Char, Unit], b: HPFLCore[Char, Char, Unit]) =
      neg(and(Set(neg(a), neg(b))))
    def biimpl(a: HPFLCore[Char, Char, Unit], b: HPFLCore[Char, Char, Unit]) =
      and(Set(or(a, b), or(neg(a), neg(b))))
    val formula = app(
      app(
        neg(mu(
          'F',
          neg(lam(
            'X',
            lam(
              'Y',
              and(Set(
                biimpl(variable('X'), variable('Y')),
                app(app(neg(variable('F')), variable('X')), variable('Y'))
              ))
            )
          ))
        )),
        top
      ),
      top
    )
    val result = typecheck(formula)
    assert(result.isDefined)
  }
