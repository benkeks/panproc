package io.equiv.panproc.hpfl


enum Variance:
  case None
  case Pos
  case Neg
  case Any

  def negate = this match
    case None => None
    case Any => Any
    case Pos => Neg
    case Neg => Pos

  def supertypes = this match
    case None => Set(None)
    case Any => Set(None, Any, Pos, Neg)
    case Pos => Set(None, Pos)
    case Neg => Set(None, Neg)

  def compose(v: Variance) = (this, v) match
    case (None, _) => None
    case (_, None) => None
    case (Any, _) => Any
    case (_, Any) => Any
    case (Pos, Pos) => Pos
    case (Neg, Neg) => Pos
    case (Pos, Neg) => Neg
    case (Neg, Pos) => Neg

  def invCompose(result: Variance) = result match
    case None => this match
      case None => Set(None, Any, Pos, Neg)
      case _ => Set(None)
    case Any => this match
      case None => Set()
      case Any => Set(Any, Pos, Neg)
      case _ => Set(Any)
    case Pos => this match
      case Pos => Set(Pos)
      case Neg => Set(Neg)
      case _ => Set()
    case Neg => this match
      case Pos => Set(Neg)
      case Neg => Set(Pos)
      case _ => Set()

val allVariances = Set(Variance.None, Variance.Pos, Variance.Neg, Variance.Any)

trait HPFLType

case object Ground extends HPFLType

case class Arrow(variance: Variance, next: HPFLType) extends HPFLType
