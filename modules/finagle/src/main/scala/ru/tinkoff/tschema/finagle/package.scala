package ru.tinkoff.tschema
import cats.effect.MonadCancel

package object finagle {
  type BracketThrow[F[_]] = MonadCancel[F, Throwable]
}
