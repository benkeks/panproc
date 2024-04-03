package io.equiv.panproc.hpfl

trait HPFLCore[A, V, L]:
  def label: L
  def replaceVar(v: V, f: HPFLCore[A, V, L]): HPFLCore[A, V, L] =
    this match
      case Variable(value, _) if value == v => f
      case Neg(subterm, _)                  => Neg(subterm.replaceVar(v, f), label)
      case And(subterms, _)                 => And(subterms.map(_.replaceVar(v, f)), label)
      case Observe(action, index, subterm, _) =>
        Observe(action, index, subterm.replaceVar(v, f), label)
      case Lambda(variable, body, _) => Lambda(variable, body.replaceVar(v, f), label)
      case Mu(variable, body, _)     => Mu(variable, body.replaceVar(v, f), label)
      case Application(transformer, argument, _) =>
        Application(transformer.replaceVar(v, f), argument.replaceVar(v, f), label)
      case _ => this

case class Top[A, V, L](label: L) extends HPFLCore[A, V, L]

def top[A, V]: HPFLCore[A, V, Unit] = Top(())

case class Variable[A, V, L](value: V, label: L) extends HPFLCore[A, V, L]

def variable[A, V](value: V): HPFLCore[A, V, Unit] = Variable(value, ())

case class Neg[A, V, L](subterm: HPFLCore[A, V, L], label: L) extends HPFLCore[A, V, L]

def neg[A, V](subterm: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] = Neg(subterm, ())

case class And[A, V, L](subterms: Set[HPFLCore[A, V, L]], label: L) extends HPFLCore[A, V, L]

def and[A, V](subterms: Set[HPFLCore[A, V, Unit]]): HPFLCore[A, V, Unit] = And(subterms, ())

def or[A, V](subterms: Set[HPFLCore[A, V, Unit]]): HPFLCore[A, V, Unit] =
  neg(and(subterms.map(st => neg(st))))

def implies[A, V](lhs: HPFLCore[A, V, Unit], rhs: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] =
  or(Set(neg(lhs), rhs))

def iff[A, V](lhs: HPFLCore[A, V, Unit], rhs: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] =
  and(Set(implies(lhs, rhs), implies(rhs, lhs)))

case class Observe[A, V, L](action: A, index: Int, subterm: HPFLCore[A, V, L], label: L)
    extends HPFLCore[A, V, L]

def obs[A, V](action: A, index: Int, subterm: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] =
  Observe(action, index, subterm, ())

case class Lambda[A, V, L](variable: V, body: HPFLCore[A, V, L], label: L) extends HPFLCore[A, V, L]

def lam[A, V](variable: V, body: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] =
  Lambda(variable, body, ())
case class Mu[A, V, L](variable: V, body: HPFLCore[A, V, L], label: L) extends HPFLCore[A, V, L]

def mu[A, V](v: V, body: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] = Mu(v, body, ())

def nu[A, V](v: V, body: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] =
  neg(mu(v, neg(body.replaceVar(v, neg(variable(v))))))

case class Application[A, V, L](
    transformer: HPFLCore[A, V, L],
    argument: HPFLCore[A, V, L],
    label: L
) extends HPFLCore[A, V, L]

def app[A, V](
    transformer: HPFLCore[A, V, Unit],
    argument: HPFLCore[A, V, Unit]
): HPFLCore[A, V, Unit] = Application(transformer, argument, ())
