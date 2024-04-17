package io.equiv.panproc.hpfl

import io.equiv.panproc.hpfl.HPFL.*

object DefaultEquivalences:
  def traceHelper1(formulas: Set[HPFL[Char, Char]]): HPFL[Char, Char] =
    Nu(
      'F',
      Lambda(
        'X',
        Lambda(
          'Y',
          And(Set(
            IFF(Variable('X'), Variable('Y'))
          ).union(formulas))
        )
      )
    )
  def apply2(
      formula: HPFL[Char, Char],
      x: HPFL[Char, Char],
      y: HPFL[Char, Char]
  ): HPFL[Char, Char] =
    Application(
      Application(formula, x),
      y
    )
  def traceHelper2(x: HPFL[Char, Char], y: HPFL[Char, Char]): HPFL[Char, Char] =
    AndOp(
      Element('a', Actions()),
      apply2(Variable('F'), x, y)
    )
  val trace: HPFL[Char, Char] =
    apply2(
      traceHelper1(Set(
        traceHelper2(
          ObsPossible('a', 1, Variable('X')),
          ObsPossible('a', 2, Variable('Y'))
        )
      )),
      Top(),
      Top()
    )
  def deadlock(index: Int): HPFL[Char, Char] =
    AndOp(Element('a', Actions()), ObsNecessary('a', index, Bot()))
  val completedTrace: HPFL[Char, Char] =
    And(
      Set(
        trace,
        apply2(
          traceHelper1(Set(
            traceHelper2(
              ObsPossible('a', 1, Variable('X')),
              ObsPossible('a', 2, Variable('Y'))
            )
          )),
          deadlock(1),
          deadlock(2)
        )
      )
    )
  def failure(index: Int, set: SetVar[Char]): HPFL[Char, Char] =
    AndOp(Element('a', set), ObsNecessary('a', index, Bot()))
  val failures: HPFL[Char, Char] =
    AndOp(
      Subset('A', Actions()),
      apply2(
        traceHelper1(Set(
          traceHelper2(
            ObsPossible('a', 1, Variable('X')),
            ObsPossible('a', 2, Variable('Y'))
          )
        )),
        failure(1, ActionsVar('A')),
        failure(2, ActionsVar('A'))
      )
    )
  val failureTrace: HPFL[Char, Char] =
    apply2(
      traceHelper1(Set(
        traceHelper2(
          ObsPossible('a', 1, Variable('X')),
          ObsPossible('a', 2, Variable('Y'))
        ),
        AndOp(
          Subset('A', Actions()),
          apply2(
            Variable('F'),
            And(Set(
              failure(1, ActionsVar('A')),
              Variable('X')
            )),
            And(Set(
              failure(2, ActionsVar('A')),
              Variable('Y')
            ))
          )
        )
      )),
      Top(),
      Top()
    )
  def ready(index: Int, set: SetVar[Char]): HPFL[Char, Char] =
    And(
      Set(
        AndOp(Element('a', set), ObsPossible('a', index, Top())),
        AndOp(Element('a', Complement(set)), ObsNecessary('a', index, Bot()))
      )
    )
  val readiness: HPFL[Char, Char] =
    AndOp(
      Subset('A', Actions()),
      apply2(
        traceHelper1(Set(
          traceHelper2(
            ObsPossible('a', 1, Variable('X')),
            ObsPossible('a', 2, Variable('Y'))
          )
        )),
        ready(1, ActionsVar('A')),
        ready(2, ActionsVar('A'))
      )
    )
  val readyTrace: HPFL[Char, Char] =
    apply2(
      traceHelper1(Set(
        traceHelper2(
          ObsPossible('a', 1, Variable('X')),
          ObsPossible('a', 2, Variable('Y'))
        ),
        AndOp(
          Subset('A', Actions()),
          apply2(
            Variable('F'),
            And(Set(
              Variable('X'),
              ready(1, ActionsVar('A'))
            )),
            And(Set(
              Variable('Y'),
              ready(2, ActionsVar('A'))
            ))
          )
        )
      )),
      Top(),
      Top()
    )
