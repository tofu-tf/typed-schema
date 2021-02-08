package ru.tinkoff.tschema.finagle

package zioRouting {
  final case class Rejected(rej: Rejection) extends Throwable
}
