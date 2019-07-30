package ru.tinkoff.tschema.finagle.routing

import ru.tinkoff.tschema.finagle.Rejection

sealed trait Fail[+E]

object Fail {
  final case class Rejected(rej: Rejection) extends Fail[Nothing]
  final case class Other[+E](err: E)    extends Fail[E]
}
