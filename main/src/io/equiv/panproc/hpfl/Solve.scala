package io.equiv.panproc.hpfl

import io.equiv.panproc.ts.*

type Relation[E] = Set[List[E]];

def cross[E](xs: List[Set[E]]): Set[List[E]] =
  xs match
    case Nil      => Set(List())
    case x :: xs1 => for x1 <- x; xs2 <- cross(xs1) yield x1 :: xs2

type ArgStack[S] = List[Relation[S]];

type VarMap[V, S] = Map[V, Relation[S]];

type MemoTable[S] = Map[ArgStack[S], Relation[S]]

type MemoMap[V, S] = Map[V, MemoTable[S]];

abstract class Solver[S, A, V, L](ltss: List[TransitionSystem[S, A, L]]):
  val completeRelation = cross(ltss.map(_.nodes))

  def pre(a: A, i: Int, r: Relation[S]): Relation[S] =
    for
      tuple <- r
      previous <- ltss(i - 1).pre(tuple(i - 1), a)
    yield tuple.updated(i - 1, previous)

  def solve(
      formula: HPFLCore.HPFLCore[A, V, HPFLType],
      stack: ArgStack[S],
      eta: VarMap[V, S],
      rho: MemoMap[V, S]
  ): Option[Relation[S]]

enum Algorithm:
  case Seq, Par

def eval[S, A, V, L](
    formula: HPFLCore.HPFLCore[A, V, HPFLType],
    ltss: List[TransitionSystem[S, A, L]],
    algorithm: Algorithm = Algorithm.Par
): Option[Relation[S]] =
  val solver: Solver[S, A, V, L] = algorithm match
    case Algorithm.Seq => SeqSolver(ltss)
    case Algorithm.Par => ParSolver(ltss)
  solver.solve(formula, List(), Map(), Map())
