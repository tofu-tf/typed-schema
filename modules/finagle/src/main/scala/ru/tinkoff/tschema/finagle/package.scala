package ru.tinkoff.tschema
import cats.effect.Bracket

package object finagle {
  type BracketThrow[F[_]] = Bracket[F, Throwable]
}
