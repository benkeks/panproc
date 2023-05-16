package io.equiv.panproc.lambda

import io.equiv.panproc.lambda.Syntax.Expression

class Environment(defs: => Map[String, Expression], parent: Environment):

  def push(newDefs: => List[(String, Expression)]): Environment =
    Environment(flatten(newDefs.toMap), null)

  def push(otherEnvironment: Environment): Environment =
    Environment(otherEnvironment.flatten(defs), null)

  def get(x: String): Option[Expression] =
    val v = defs.get(x)
    if parent ne null then
      v.orElse(parent.get(x))
    else
      v

  def height(): Int =
    if parent == null then
      1
    else
      1 + parent.height()

  def flatten(collected: Map[String, Expression] = Map()): Map[String, Expression] =
    val relevantDefs = defs -- collected.keys
    val newCollected = collected ++ relevantDefs
    if parent == null then
      newCollected
    else
      parent.flatten(newCollected)

  override def hashCode(): Int = flatten().keySet.hashCode()

  override def equals(other: Any): Boolean =
    other match
      case e: Environment =>
        e.flatten().keySet == this.flatten().keySet
      case _ =>
        false

  override def toString() = s"${height()}: ${defs.keys.mkString(",")}"
