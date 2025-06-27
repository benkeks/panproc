package io.equiv.panproc.hpfl

case class TypeEnvironment[V](env: Map[V, (Variance, HPFLType)]):
  def negate = TypeEnvironment(env.map((v, el) => (v, (el._1.negate, el._2))))
  def superenvs: Set[TypeEnvironment[V]] =
    env
      .foldLeft(Set(emptyTypeEnvironment))((acc, el) =>
        for
          acc1 <- acc
          v <- el._2._1.supertypes
        yield acc1.updated(el._1, (v, el._2._2))
      )
  def invCompose(v: Variance): Set[TypeEnvironment[V]] =
    env
      .foldLeft(Set(emptyTypeEnvironment))((acc, el) =>
        for
          acc1 <- acc
          v1 <- v.invCompose(el._2._1)
        yield acc1.updated(el._1, (v1, el._2._2))
      )

  def updated(v: V, el: (Variance, HPFLType)) = TypeEnvironment(env.updated(v, el))
  def get(v: V) = env.get(v)
  def contains(v: V) = env.contains(v)

def emptyTypeEnvironment[V]: TypeEnvironment[V] = TypeEnvironment(Map())

def sequence[A](xs: Iterable[Option[A]]): Option[List[A]] =
  if xs.isEmpty
  then Some(List())
  else
    for
      x1 <- xs.head
      xs2 <- sequence(xs.tail)
    yield x1 :: xs2

def firstSome[A, B](xs: Iterable[A], f: A => Option[B]): Option[B] =
  if xs.isEmpty then None
  else
    f(xs.head) match
      case None        => firstSome(xs.tail, f)
      case Some(value) => Some(value)

def typecheck[A, V, L](
    formula: HPFLCore.HPFLCore[A, V, L],
    dtype: HPFLType = Ground,
    env: TypeEnvironment[V] = emptyTypeEnvironment[V]
): Option[HPFLCore.HPFLCore[A, V, HPFLType]] =
  formula match
    case HPFLCore.Top(_) => if dtype == Ground then Some(HPFLCore.Top(Ground)) else None

    case HPFLCore.Variable(value, _) =>
      for
        (variance, vtype) <- env.get(value)
        if variance == Variance.Any || variance == Variance.Pos
        if dtype == vtype
      yield HPFLCore.Variable(value, dtype)

    case HPFLCore.Neg(subterm, _) =>
      for
        subterm1 <- typecheck(subterm, dtype, env.negate)
      yield HPFLCore.Neg(subterm1, dtype)

    case HPFLCore.And(subterms, _) =>
      if dtype == Ground then
        for
          subterms1 <- sequence(subterms.map(typecheck(_, dtype, env)).toList)
        yield HPFLCore.And(subterms1.toSet, dtype)
      else None

    case HPFLCore.ObsPossible(action, index, subterm, _) =>
      if dtype == Ground then
        for
          subterm1 <- typecheck(subterm, dtype, env)
        yield HPFLCore.ObsPossible(action, index, subterm1, dtype)
      else None

    case HPFLCore.Lambda(variable, body, _) =>
      dtype match
        case Arrow(variance, next) =>
          for
            body1 <- typecheck(body, next, env.updated(variable, (variance, Ground)))
          yield HPFLCore.Lambda(variable, body1, dtype)
        case Ground => None

    case HPFLCore.Mu(variable, body, _) =>
      for
        body1 <- typecheck(body, dtype, env.updated(variable, (Variance.Pos, dtype)))
      yield HPFLCore.Mu(variable, body1, dtype)

    case HPFLCore.Application(transformer, argument, _) =>
      val envs1 = env.superenvs
      firstSome(
        allVariances,
        (v) =>
          val envs2 = envs1.flatMap(_.invCompose(v))
          for
            transformer1 <- firstSome(envs1, env1 => typecheck(transformer, Arrow(v, dtype), env1))
            argument1 <- firstSome(envs2, env2 => typecheck(argument, Ground, env2))
          yield HPFLCore.Application(transformer1, argument1, dtype)
      )
