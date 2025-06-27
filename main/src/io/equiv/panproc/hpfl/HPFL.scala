package io.equiv.panproc.hpfl

import io.equiv.panproc.ts.*

object HPFL:
  trait SetVar[AV]

  case class Actions[AV]() extends SetVar[AV]

  case class ActionsVar[AV](value: AV) extends SetVar[AV]

  case class Complement[AV](set: SetVar[AV]) extends SetVar[AV]

  trait Operator[AV]

  case class Subset[AV](subset: AV, superset: SetVar[AV]) extends Operator[AV]

  case class Element[AV](element: AV, set: SetVar[AV]) extends Operator[AV]

  trait HPFL[AV, V]:
    def instantiate[S, A, L](
        ltss: List[TransitionSystem[S, A, L]],
        sets: Map[AV, Set[A]] = Map(),
        actions: Map[AV, A] = Map()
    ): Option[HPFLCore.HPFLCore[A, V, Unit]] =
      def getSet(setvar: SetVar[AV]): Option[Set[A]] =
        setvar match
          case Actions() => Some(ltss.flatMap(_.actions).toSet)
          case ActionsVar(value) => sets.get(value)
          case Complement(set) =>
            for
              set1 <- getSet(set)
            yield ltss.flatMap(_.actions).toSet -- set1
      def instantiateSubterms(op: Operator[AV], formula: HPFL[AV,V]): Option[Set[HPFLCore.HPFLCore[A,V,Unit]]] =
        op match
          case Subset(subset, superset) =>
            for
              superset1 <- getSet(superset)
              subterms <- sequence(superset1.subsets().map(subset1 => formula.instantiate(ltss, sets + (subset -> subset1), actions)).to(Iterable))
            yield subterms.toSet
          case Element(element, set) =>
            for
              set1 <- getSet(set)
              subterms <- sequence(set1.map(element1 => formula.instantiate(ltss, sets, actions + (element -> element1))))
            yield subterms.toSet
      this match
        case Top() => Some(HPFLCore.top)
        case Bot() => Some(HPFLCore.neg(HPFLCore.top))
        case Variable(value) => Some(HPFLCore.variable(value))
        case Neg(subterm) =>
          for
            subterm1 <- subterm.instantiate(ltss, sets, actions)
          yield HPFLCore.neg(subterm1)
        case And(subterms) =>
          for
            subterms1 <- sequence(subterms.map(_.instantiate(ltss, sets, actions)))
          yield HPFLCore.and(subterms1.toSet)
        case Or(subterms) =>
          for
            subterms1 <- sequence(subterms.map(_.instantiate(ltss, sets, actions)))
          yield HPFLCore.or(subterms1.toSet)
        case AndOp(op, subterm) =>
          for
            subterms <- instantiateSubterms(op, subterm)
          yield HPFLCore.and(subterms)
        case OrOp(op, subterm) =>
          for
            subterms <- instantiateSubterms(op, subterm)
          yield HPFLCore.or(subterms)
        case Implies(a, b) =>
          for
            a1 <- a.instantiate(ltss, sets, actions)
            b1 <- b.instantiate(ltss, sets, actions)
          yield HPFLCore.implies(a1, b1)
        case IFF(a, b) =>
          for
            a1 <- a.instantiate(ltss, sets, actions)
            b1 <- b.instantiate(ltss, sets, actions)
          yield HPFLCore.iff(a1, b1)
        case ObsPossible(action, index, subterm) =>
          for
            a <- actions.get(action)
            subterm1 <- subterm.instantiate(ltss, sets, actions)
          yield HPFLCore.obs(a, index, subterm1)
        case ObsNecessary(action, index, subterm) =>
          for
            a <- actions.get(action)
            subterm1 <- subterm.instantiate(ltss, sets, actions)
          yield HPFLCore.nec(a, index, subterm1)
        case Lambda(variable, body) =>
          for
            body1 <- body.instantiate(ltss, sets, actions)
          yield HPFLCore.lam(variable, body1)
        case Mu(variable, body) =>
          for
            body1 <- body.instantiate(ltss, sets, actions)
          yield HPFLCore.mu(variable, body1)
        case Nu(variable, body) =>
          for
            body1 <- body.instantiate(ltss, sets, actions)
          yield HPFLCore.nu(variable, body1)
        case Application(transformer, argument) =>
          for
            transformer1 <- transformer.instantiate(ltss, sets, actions)
            argument1 <- argument.instantiate(ltss, sets, actions)
          yield HPFLCore.app(transformer1, argument1)
    def check[A, V, AV, S, L](
      ltss: List[TransitionSystem[S, A, L]],
      algorithm: Algorithm = Algorithm.Seq
    ): Option[Relation[S]] =
      for
        instance <- this.instantiate(ltss)
        formula <- typecheck(instance, Ground)
        result <- eval(formula, ltss, algorithm)
      yield result

  case class Top[AV, V]() extends HPFL[AV, V]

  case class Bot[AV, V]() extends HPFL[AV, V]

  case class Variable[AV, V](value: V) extends HPFL[AV, V]

  case class Neg[AV, V](subterm: HPFL[AV, V]) extends HPFL[AV, V]

  case class And[AV, V](subterms: Set[HPFL[AV, V]]) extends HPFL[AV, V]

  case class Or[AV, V](subterms: Set[HPFL[AV, V]]) extends HPFL[AV, V]

  case class AndOp[AV, V](op: Operator[AV], subterm: HPFL[AV, V]) extends HPFL[AV, V]

  case class OrOp[AV, V](op: Operator[AV], subterm: HPFL[AV, V]) extends HPFL[AV, V]

  case class Implies[AV, V](a: HPFL[AV, V], b: HPFL[AV, V]) extends HPFL[AV, V]

  case class IFF[AV, V](a: HPFL[AV, V], b: HPFL[AV, V]) extends HPFL[AV, V]

  case class ObsPossible[AV, V](action: AV, index: Int, subterm: HPFL[AV, V]) extends HPFL[AV, V]

  case class ObsNecessary[AV, V](action: AV, index: Int, subterm: HPFL[AV, V]) extends HPFL[AV, V]

  case class Lambda[AV, V](variable: V, body: HPFL[AV, V]) extends HPFL[AV, V]

  case class Mu[AV, V](variable: V, body: HPFL[AV, V]) extends HPFL[AV, V]

  case class Nu[AV, V](variable: V, body: HPFL[AV, V]) extends HPFL[AV, V]

  case class Application[AV, V](transformer: HPFL[AV, V], argument: HPFL[AV, V]) extends HPFL[AV, V]


