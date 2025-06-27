package io.equiv.panproc.hpfl

import io.equiv.panproc.hpfl.HPFL.*

// all formulae are taken from https://www.sciencedirect.com/science/article/pii/S0304397514006574
object DefaultEquivalences:
  // nu F.\X.\Y.(X <-> Y) ^ ^(formulas...)
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

  // formula x y
  def apply2(
      formula: HPFL[Char, Char],
      x: HPFL[Char, Char],
      y: HPFL[Char, Char]
  ): HPFL[Char, Char] =
    Application(
      Application(formula, x),
      y
    )

  // ^a in Act. F x y
  def traceHelper2(x: HPFL[Char, Char], y: HPFL[Char, Char]): HPFL[Char, Char] =
    AndOp(
      Element('a', Actions()),
      apply2(Variable('F'), x, y)
    )

  // Proposition 7
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

  // Proposition 8
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

  // Proposition 9
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

  // Proposition 10
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

  // Proposition 11
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

  // Proposition 12
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

  // ^a in Act. [a]index1 <a>index2 varName
  def simulationHelper(index1: Int, index2: Int, varName: Char): HPFL[Char, Char] =
    AndOp(
      Element('a', Actions()),
      ObsNecessary('a', index1, ObsPossible('a', index2, Variable(varName)))
    )

  // Proposition 13
  val simulation: HPFL[Char, Char] =
    And(Set(
      Nu('X', simulationHelper(1, 2, 'X')),
      Nu('Y', simulationHelper(2, 1, 'Y'))
    ))

  // Proposition 14
  val completedSimulation: HPFL[Char, Char] =
    And(Set(
      Nu(
        'X',
        And(Set(
          IFF(deadlock(1), deadlock(2)),
          simulationHelper(1, 2, 'X')
        ))
      ),
      Nu(
        'Y',
        And(Set(
          IFF(deadlock(2), deadlock(1)),
          simulationHelper(2, 1, 'Y')
        ))
      )
    ))

  // Proposition 15
  val readySimulation: HPFL[Char, Char] =
    And(Set(
      Nu(
        'X',
        And(Set(
          AndOp(Subset('A', Actions()), IFF(ready(1, ActionsVar('A')), ready(2, ActionsVar('A')))),
          simulationHelper(1, 2, 'X')
        ))
      ),
      Nu(
        'Y',
        And(Set(
          AndOp(Subset('A', Actions()), IFF(ready(2, ActionsVar('A')), ready(1, ActionsVar('A')))),
          simulationHelper(2, 1, 'Y')
        ))
      )
    ))

  // Proposition 16
  val twoNestedSimulation: HPFL[Char, Char] =
    And(Set(
      Nu(
        'U',
        And(Set(
          simulation,
          simulationHelper(1, 2, 'U')
        ))
      ),
      Nu(
        'V',
        And(Set(
          simulation,
          simulationHelper(2, 1, 'V')
        ))
      )
    ))

  // Proposition 17
  val bisimulation: HPFL[Char, Char] =
    Nu(
      'X',
      And(Set(
        simulationHelper(1, 2, 'X'),
        simulationHelper(2, 1, 'X')
      ))
    )
