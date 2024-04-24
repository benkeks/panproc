package io.equiv.panproc.hpfl

case class TypeEnvironment[V](env: Map[V, (Variance, HPFLType)]):
  def negate = TypeEnvironment(env.map((v, el) => (v, (el._1.negate, el._2))))
  def supertypes: Set[TypeEnvironment[V]] =
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
        yield acc1.updated(el._1, (v, el._2._2))
      )

  def updated(v: V, el: (Variance, HPFLType)) = TypeEnvironment(env.updated(v, el))
  def get(v: V) = env.get(v)
  def contains(v: V) = env.contains(v)

def emptyTypeEnvironment[V]: TypeEnvironment[V] = TypeEnvironment(Map())

def sequence[A](xs: Iterable[Option[A]]): Option[List[A]] =
  xs.foldRight[Option[List[A]]](Some(Nil))((x, acc) => for (x1 <- x; acc1 <- acc) yield x1 :: acc1)

def firstSome[A, B](xs: Iterable[A], f: A => Option[B]): Option[B] =
  xs.foldRight[Option[B]](None)((x, acc) => f(x).orElse(acc))

def typecheck[A, V, L](
    expr: HPFLCore.HPFLCore[A, V, L],
    dtype: HPFLType = Ground,
    env: TypeEnvironment[V] = emptyTypeEnvironment[V]
): Option[HPFLCore.HPFLCore[A, V, HPFLType]] =
  expr match
    case HPFLCore.Top(_) => if dtype == Ground then Some(HPFLCore.Top(Ground)) else None

    case HPFLCore.Variable(value, _) =>
      for
        (variance, vtype) <- env.get(value)
        if variance == Variance.Any || variance == Variance.Pos
      yield HPFLCore.Variable(value, dtype)

    case HPFLCore.Neg(subterm, _) =>
      for
        subterm1 <- typecheck(subterm, dtype, env.negate)
      yield HPFLCore.Neg(subterm1, dtype)

    case HPFLCore.And(subterms, _) =>
      if dtype == Ground then
        for
          subterms1 <- sequence(subterms.map(typecheck(_, dtype, env)))
        yield HPFLCore.And(subterms1.toSet, dtype)
      else None

    case HPFLCore.Observe(action, index, subterm, _) =>
      if dtype == Ground then
        for
          subterm1 <- typecheck(subterm, dtype, env)
        yield HPFLCore.Observe(action, index, subterm1, dtype)
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
      val envs1 = env.supertypes
      def envs2(v: Variance) = env.invCompose(v)
      allVariances.foldLeft[Option[HPFLCore.HPFLCore[A, V, HPFLType]]](None)((acc, v) =>
        acc.orElse(
          for
            transformer1 <- firstSome(envs1, env => typecheck(transformer, Arrow(v, dtype), env))
            argument1 <- firstSome(envs2(v), env => typecheck(argument, Ground, env))
          yield HPFLCore.Application(transformer1, argument1, dtype)
        )
      )
