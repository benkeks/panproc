package io.equiv.panproc.parsing

object Parsing {
  abstract class AbstractExpression
}

/** Interpreting classes are meant to to assign a denotation Success[E] to 
 *  every meaningful expression or return a Problem pointing the erroneous
 *  sub expressions.*/
object Interpreting {
  
  sealed abstract class Result[+E] {
    def isProblem: Boolean
    
    def get: E
    
    def getProblem: Option[Problem]
    
    final def map[B](f: E => B): Result[B] = this match {
      case p1 @ Problem(msg, expr) =>
        p1
      case Success(res1) =>
        Success(f(res1))
    }
    
    final def flatMap[B](f: E => Result[B]): Result[B] = this match {
      case p1 @ Problem(msg, expr) =>
        p1
      case Success(res1) =>
        f(res1)
    }
    
    final def filter(f: E => Boolean, msg: String=""): Result[E] =
      flatMap {e => if (f(e)) Success(e) else Problem(msg, List())}
    
    /**
     * combines two success-results using a function. if one of the results is
     * a problem, it will oust the successes.
     */
    final def combine[E1, E2](f: (E,E1) => E2, other: Result[E1]): Result[E2] = this match {
      case p1 @ Problem(msg, expr) => other match {
        case Problem(msg2, expr2) => Problem(msg + "\nAND "+msg2, expr ++ expr2)
        case _ => p1
      }
      case Success(res1) => other match {
        case o: Problem => o
        case Success(res2) => Success(f(res1, res2))
      }
    }
  }
  
  case class Problem(msg: String, expr: List[Parsing.AbstractExpression]) extends Result[Nothing] {
    
    override def isProblem = true
    
    override def get = throw new Error("Can't get result from syntax. Problem: " + msg)
    
    override def getProblem = Some(this)
    
    def combineProblem(other: Problem) =
      val Problem(msg2, expr2) = other
      Problem(msg + "\nAND "+msg2, expr ++ expr2)
    
    override def toString: String = msg
  }
  
  case class Success[+E](result: E) extends Result[E] {
    override def isProblem = false
    
    override def get = result
    
    override def getProblem = None
  }
  
  def partitionResults[E](results: Iterable[Result[E]]) = {
    val probs = results.collect {case p: Problem => p}
    val mergedProbs = if (probs.nonEmpty) Some(probs reduce (_ combineProblem _)) else None 
    (results.collect {case Success(r) => r}, mergedProbs)
  }
  
  def factorResults[E](results: Iterable[Result[E]]): Result[Iterable[E]] = {
    val (suc, prob) = partitionResults(results)
    prob.getOrElse(Success(suc))
  }

  def fromOption[E](opt: Option[E]) = opt match {
    case Some(value) => Success(value)
    case None => Problem("Empty option error", List())
  }
}