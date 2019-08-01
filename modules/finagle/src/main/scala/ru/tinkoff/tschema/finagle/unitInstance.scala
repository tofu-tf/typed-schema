package ru.tinkoff.tschema.finagle
import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.apply._
import com.twitter.finagle.http.{Response, Status}
import ru.tinkoff.tschema.finagle.util.message

object unitInstance {
  implicit def unitCompleteInstance[F[_]: Applicative, G[_]]: Complete[F, Unit, Unit] = message.emptyComplete()
  implicit def unitCompleteFInstance[F[_]: Applicative, G[_]](
      implicit lift: LiftHttp[F, G]
  ): Complete[F, Unit, G[Unit]] =
    in => lift(in) *> Response(Status.Ok).pure[F]
}
