package io.equiv.panproc.hpfl
import io.equiv.panproc.ts.*
import scala.collection.parallel.CollectionConverters.*

class ParSolver[S, A, V, L](ltss: List[TransitionSystem[S, A, L]]) extends Solver[S, A, V, L](ltss):
  // used for benchmarking
  var table_stats: List[(Int, Int)] = List()

  def _solve(
      formula: HPFLCore.HPFLCore[A, V, HPFLType],
      stack: ArgStack[S],
      eta: VarMap[V, S],
      rho: MemoMap[V, S]
  ): Either[Set[ArgStack[S]], Relation[S]] =
    formula match
      case HPFLCore.Top(_)              => Right(completeRelation)
      case HPFLCore.Variable(v, Ground) => Right(eta(v))
      case HPFLCore.Variable(v, _) =>
        val t = rho(v)
        if t.contains(stack)
        then Right(t(stack))
        else Left(Set(stack))
      case HPFLCore.Neg(f, _) =>
        for
          r <- _solve(f, stack, eta, rho)
        yield completeRelation -- r
      case HPFLCore.And(fs, _) =>
        fs.par.map(_solve(_, stack, eta, rho)).fold(Right(completeRelation)) {
          case (Right(r1), Right(r2)) => Right(r1 & r2)
          case (Right(r1), Left(s))   => Left(s)
          case (Left(s), Right(r2))   => Left(s)
          case (Left(s1), Left(s2))   => Left(s1 ++ s2)
        }
      case HPFLCore.ObsPossible(a, i, f, _) =>
        for
          r <- _solve(f, stack, eta, rho)
        yield pre(a, i, r)
      case HPFLCore.Lambda(v, f, _) =>
        _solve(f, stack.tail, eta.updated(v, stack.head), rho)
      case HPFLCore.Mu(v, f, Ground) =>
        var x = Right(Set()): Either[Set[ArgStack[S]], Relation[S]]
        while
          val x_old = x
          x =
            for
              r1 <- x
              r2 <- _solve(f, stack, eta.updated(v, r1), rho)
            yield r2
          x != x_old
        do ()
        x
      case HPFLCore.Mu(v, f, _) =>
        def updateTable(t: MemoTable[S]): MemoTable[S] =
          val nextEntries = t.keySet.par.flatMap { stack1 =>
            _solve(f, stack1, eta, rho + (v -> t)) match
              case Right(r)     => Set(stack1 -> r)
              case Left(stacks) => Set(stack1 -> Set()) ++ stacks.map(_ -> Set())
          }.seq
          Map.from(nextEntries)

        var iters = 0
        var t = Map(stack -> Set()): MemoTable[S]
        while
          val t_old = t
          t = updateTable(t)
          iters += 1
          t != t_old
        do ()
        table_stats = (t.size, iters) :: table_stats
        Right(t(stack))
      case HPFLCore.Application(f1, f2, _) =>
        for
          r2 <- _solve(f2, stack, eta, rho)
          r1 <- _solve(f1, r2 :: stack, eta, rho)
        yield r1

  def solve(
      formula: HPFLCore.HPFLCore[A, V, HPFLType],
      stack: ArgStack[S],
      eta: VarMap[V, S],
      rho: MemoMap[V, S]
  ): Option[Relation[S]] =
    _solve(formula, stack, eta, rho) match
      case Right(r) => Some(r)
      case Left(_)  => None
