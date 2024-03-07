package io.equiv.panproc.hpfl

trait HPFLCore[A, V, L]:
  def label: L

case class Top[A, V, L](label: L) extends HPFLCore[A, V, L]

def top[A, V]: HPFLCore[A, V, Unit] = Top(())

case class Variable[A, V, L](value: V, label: L) extends HPFLCore[A, V, L]

def variable[A, V](value: V): HPFLCore[A, V, Unit] = Variable(value, ())

case class Neg[A, V, L](subterm: HPFLCore[A, V, L], label: L) extends HPFLCore[A, V, L]

def neg[A, V](subterm: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] = Neg(subterm, ())

case class And[A, V, L](subterms: Set[HPFLCore[A, V, L]], label: L) extends HPFLCore[A, V, L]

def and[A, V](subterms: Set[HPFLCore[A, V, Unit]]): HPFLCore[A, V, Unit] = And(subterms, ())
case class Observe[A, V, L](action: A, index: Int, subterm: HPFLCore[A, V, L], label: L)
    extends HPFLCore[A, V, L]

def obs[A, V](action: A, index: Int, subterm: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] =
  Observe(action, index, subterm, ())

case class Lambda[A, V, L](variable: V, body: HPFLCore[A, V, L], label: L) extends HPFLCore[A, V, L]

def lam[A, V](variable: V, body: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] = Lambda(variable, body, ())
case class Mu[A, V, L](variable: V, body: HPFLCore[A, V, L], label: L) extends HPFLCore[A, V, L]

def mu[A, V](variable: V, body: HPFLCore[A, V, Unit]): HPFLCore[A, V, Unit] = Mu(variable, body, ())

case class Application[A, V, L](
    transformer: HPFLCore[A, V, L],
    argument: HPFLCore[A, V, L],
    label: L
) extends HPFLCore[A, V, L]

def app[A, V](
    transformer: HPFLCore[A, V, Unit],
    argument: HPFLCore[A, V, Unit]
): HPFLCore[A, V, Unit] = Application(transformer, argument, ())
