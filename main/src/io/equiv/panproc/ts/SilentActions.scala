package io.equiv.panproc.ts

trait SilentActions[A] {
  val silentActions: Set[A]
}
