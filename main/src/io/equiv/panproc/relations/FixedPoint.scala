package io.equiv.panproc.relations

import scala.collection.mutable.Builder

class FixedPoint[A](step: (A) => A, goodEnough: (A,A) => Boolean) {
  def apply(initial: A): A = {
    val newValue = step(initial)
    if (goodEnough(initial, newValue)) {
      newValue
    } else {
      apply(newValue)
    }
  }
  
  def applyIfNecessary(initial: A): A = {
    if (goodEnough(initial, initial)) {
      initial
    } else {
      apply(initial)
    }
  }
}