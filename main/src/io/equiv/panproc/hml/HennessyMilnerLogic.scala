package io.equiv.panproc.hml

object HennessyMilnerLogic:

  val OutputSugaredWeakFormulas = false

  abstract class Formula[A]:

    def isPositive: Boolean

    def isImmediate: Boolean


  case class Placeholder[A](name: String) extends Formula[A]:
    def isPositive: Boolean = false
    def isImmediate: Boolean = false

  case class And[A](subterms: Set[Formula[A]]) extends Formula[A]:

    override def toString =
      if subterms.isEmpty then
        "⊤"
      else
        subterms.mkString("⋀{", ",", "}")
    override val isPositive = true

    override val isImmediate = subterms.nonEmpty

    def mergeWith(other: Formula[A]) = other match
      case And(subterms) => And(this.subterms ++ subterms)
      case miscellaneous => And(this.subterms + miscellaneous)

  def True[A]: And[A] = And[A](Set())

  case class Observe[A](action: A, andThen: Formula[A]) extends Formula[A]:

    override def toString = "⟨" + action.toString + "⟩" + andThen.toString

    override val isPositive = true

    override val isImmediate = true

  case class ObserveInternal[A](andThen: Formula[A], opt: Boolean = false) extends Formula[A]:

    override def toString = (if opt then "(τ)" else "⟨τ⟩") + andThen.toString

    override val isPositive = true

    override val isImmediate = true

  case class Pass[A](andThen: Formula[A]) extends Formula[A]:

    override def toString = andThen match
      case And(subterms) if OutputSugaredWeakFormulas && subterms.nonEmpty =>
        subterms.mkString("⨇{", ",", "}")
      case And(subterms) if subterms.isEmpty => "⊤"
      case Observe(action, andThen) if OutputSugaredWeakFormulas =>
        "⟪" + action.toString + "⟫" + andThen.toString
      case _ => "⟨ϵ⟩" + andThen.toString

    override val isPositive = andThen.isPositive

    override val isImmediate = false

  case class Negate[A](andThen: Formula[A]) extends Formula[A]:

    override def toString = "¬" + andThen.toString

    override val isPositive = false

    override val isImmediate = andThen.isImmediate
