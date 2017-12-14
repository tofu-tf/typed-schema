package ru.tinkoff.tschema.utils

import cats.Eval
import monocle.PSetter
import cats.instances.map._

object setters {
  def map[K, A] = PSetter.fromFunctor[Map[K, ?], A, A]
  def eval[A] = PSetter.fromFunctor[Eval, A, A]
}
