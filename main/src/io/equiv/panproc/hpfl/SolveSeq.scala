package io.equiv.panproc.hpfl
import io.equiv.panproc.ts.*

class SeqSolver[S, A, V, L](ltss: List[TransitionSystem[S, A, L]]) extends Solver[S, A, V, L](ltss):
  // used for benchmarking
  var table_stats: List[(Int, Int)] = List()

  def _solve(
    formula: HPFLCore.HPFLCore[A, V, HPFLType],
    stack: ArgStack[S],
    eta: VarMap[V, S],
    rho: MemoMap[V, S]
  ): Either[ArgStack[S], Relation[S]] =
    formula match
      case HPFLCore.Top(_) => Right(completeRelation)
      case HPFLCore.Variable(v, Ground) => Right(eta(v))
      case HPFLCore.Variable(v, _) =>
        val t = rho(v)
        if t.contains(stack)
        then Right(t(stack))
        else Left(stack)
      case HPFLCore.Neg(f, _) =>
        for
          r <- _solve(f, stack, eta, rho)
        yield completeRelation -- r
      case HPFLCore.And(fs, _) =>
        var r: Either[ArgStack[S], Relation[S]] = Right(completeRelation)
        for f <- fs do
          r = for
            r1 <- r
            r2 <- _solve(f, stack, eta, rho)
          yield r1 & r2
        r
      case HPFLCore.ObsPossible(a, i, f, _) =>
        for
          r <- _solve(f, stack, eta, rho)
        yield pre(a, i, r)
      case HPFLCore.Lambda(v, f, _) =>
        _solve(f, stack.tail, eta.updated(v, stack.head), rho)
      case HPFLCore.Mu(v, f, Ground) =>
        var x = Right(Set()): Either[ArgStack[S], Relation[S]]
        while
          val x_old = x
          x = for
            r1 <- x
            r2 <- _solve(f, stack, eta.updated(v, r1), rho)
          yield r2
          x != x_old
        do ()
        x
      case HPFLCore.Mu(v, f, _) =>
        def updateTable(t: MemoTable[S]): MemoTable[S] =
          val nextEntries = for
            stack1 <- t.keySet
            entry <- _solve(f, stack1, eta, rho + (v -> t)) match
              case Right(r)         => Set(stack1 -> r)
              case Left(stack2) => Set(stack1 -> Set(), stack2 -> Set())
          yield entry
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
      case Left(_) => None
