package ru.tinkoff.tschema.finagle.envRouting

import ru.tinkoff.tschema.finagle.Rejection

final case class Rejected(rej: Rejection) extends Throwable
