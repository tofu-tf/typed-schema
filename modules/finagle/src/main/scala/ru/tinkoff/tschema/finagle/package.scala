package ru.tinkoff.tschema
import cats.effect.Bracket

package object finagle {
  type BracketThrow[F[_]] = Bracket[F, Throwable]

  @deprecated("use tschema.finagle.MkService", since = "0.12.1")
  val MkService: tschema.finagle.MkService.type = tschema.finagle.MkService
}
