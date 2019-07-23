package ru.tinkoff.tschema.finagle
import cats.syntax.show._
import cats.{Applicative, Show}
import ru.tinkoff.tschema.finagle.util.message

object showInstances {
  implicit def showCompleteInstance[F[_]: Applicative, A: Show]: Complete[F, A]     = message.stringComplete(_.show)
  implicit def showCompleteFInstance[F[_]: Applicative, A: Show]: Complete[F, F[A]] = message.fstringComplete(_.show)
}
