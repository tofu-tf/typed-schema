package ru.tinkoff.tschema.finagle
package routing

final case class Rejected(rej: Rejection) extends Throwable
