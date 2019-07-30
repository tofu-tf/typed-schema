package ru.tinkoff.tschema.utils

import cats.Eval
import monocle.{PSetter, Setter}

object setters {
  def eval[A]: Setter[Eval[A], A] = PSetter(f => _.map(f))
}
