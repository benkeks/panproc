package io.equiv.panproc.hpfl

import io.equiv.panproc.hpfl.HPFL.*

object DefaultEquivalences:
  val trace: HPFL[Char, Char] =
    Application(
      Application(
        Nu(
          'F',
          Lambda(
            'X',
            Lambda(
              'Y',
              And(Set(
                IFF(Variable('X'), Variable('Y')),
                AndOp(
                  Element('a', Actions()),
                  Application(
                    Application(Variable('F'), ObsPossible('a', 1, Variable('X'))),
                    ObsPossible('a', 2, Variable('Y'))
                  )
                )
              ))
            )
          )
        ),
        Top()
      ),
      Top()
    )
