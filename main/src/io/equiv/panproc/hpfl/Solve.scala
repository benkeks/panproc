package io.equiv.panproc.hpfl
import io.equiv.panproc.ts.*

type Relation[E] = Set[List[E]];

def cross[E](xs: List[Set[E]]): Set[List[E]] =
  xs match
    case Nil      => Set(List())
    case x :: xs1 => for x1 <- x; xs2 <- cross(xs1) yield x1 :: xs2

trait Result[V, E]:
  def map[V1](f: V => V1): Result[V1, E] =
    this match
      case Ok(v)    => Ok(f(v))
      case Error(e) => Error(e)
  def flatMap[V1](f: V => Result[V1, E]): Result[V1, E] =
    this match
      case Ok(v)    => f(v)
      case Error(e) => Error(e)

case class Ok[V, E](value: V) extends Result[V, E];

case class Error[V, E](value: E) extends Result[V, E];

type EvalStack[S] = List[Relation[S]];

type EvalEta[V, S] = Map[V, Relation[S]];

type EvalTable[S] = Map[EvalStack[S], Relation[S]]

type EvalRho[V, S] = Map[V, EvalTable[S]];

def fixM[V, E](f: V => Result[V, E], v: V): Result[V, E] =
  f(v).flatMap(v1 => if v == v1 then Ok(v) else fixM(f, v1))

def eval[S, A, V, L](
    formula: HPFLCore.HPFLCore[A, V, HPFLType],
    stack: EvalStack[S],
    eta: EvalEta[V, S],
    rho: EvalRho[V, S],
    ltss: List[TransitionSystem[S, A, L]]
): Result[Relation[S], EvalStack[S]] =
  val completeRelation = cross(ltss.map(_.nodes))
  formula match
    case HPFLCore.Top(_) => Ok(completeRelation)
    case HPFLCore.Variable(v, dt) =>
      if dt == Ground
      then Ok(eta(v))
      else
        val t = rho(v)
        if t.contains(stack)
        then Ok(t(stack))
        else Error(stack)
    case HPFLCore.Neg(f, _) =>
      for
        r <- eval(f, stack, eta, rho, ltss)
      yield completeRelation -- r
    case HPFLCore.And(fs, _) =>
      fs.foldLeft(Ok(completeRelation): Result[Relation[S], EvalStack[S]])((acc, f) =>
        for
          r1 <- acc
          r2 <- eval(f, stack, eta, rho, ltss)
        yield r1 & r2
      )
    case HPFLCore.Observe(a, i, f, _) =>
      for
        r <- eval(f, stack, eta, rho, ltss)
      yield r.flatMap(e => ltss(i - 1).pre(e(i - 1), a).map(e.updated(i - 1, _)))
    case HPFLCore.Lambda(v, f, _) =>
      eval(f, stack.tail, eta.updated(v, stack.head), rho, ltss)
    case HPFLCore.Mu(v, f, Ground) =>
      fixM(r => eval(f, stack, eta.updated(v, r), rho, ltss), Set())
    case HPFLCore.Mu(v, f, _) =>
      for
        t <- fixM(
          (t: EvalTable[S]) =>
            Ok(t.keySet.toList.foldLeft(Map(): EvalTable[S])((tacc, stack1) =>
              eval(f, stack1, eta, rho.updated(v, t), ltss) match
                case Ok(r)         => tacc + (stack1 -> r)
                case Error(stack2) => tacc + (stack1 -> Set()) + (stack2 -> Set())
            )),
          Map(stack -> Set())
        )
      yield t(stack)
    case HPFLCore.Application(f1, f2, _) =>
      for
        r2 <- eval(f2, stack, eta, rho, ltss)
        r1 <- eval(f1, r2 :: stack, eta, rho, ltss)
      yield r1
