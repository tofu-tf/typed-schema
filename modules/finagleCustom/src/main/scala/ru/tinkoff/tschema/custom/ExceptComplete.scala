package ru.tinkoff.tschema.custom
import cats.syntax.applicative._
import cats.{Applicative, ApplicativeError, Monad}
import com.twitter.finagle.http.{Response, Status}
import ru.tinkoff.tschema.finagle.{Complete, LiftHttp}

trait ExceptComplete[F[_], R, E, A] extends Complete[F, ExceptResult[E, R], A]

object ExceptComplete extends ExceptCompleteInstances with ExceptPureCompletes {
  implicit def optionComplete[F[_], G[_], R, A](
      implicit F: Monad[F],
      success: Complete[F, R, A],
      lift: LiftHttp[F, G]
  ): ExceptComplete[F, R, None.type, G[Option[A]]] =
    fa => F.flatMap(lift(fa))(_.fold(Response(Status.NotFound).pure[F])(success.complete))

  implicit def pureEitherComplete[F[_], G[_], E, R, A](
      implicit F: Monad[F],
      success: Complete[F, R, A],
      err: AsResponse.Error[E],
      lift: LiftHttp[F, G]
  ): ExceptComplete[F, R, E, G[Either[E, A]]] =
    fa => F.flatMap(lift(fa))(_.fold(err(_).pure[F], success.complete))
}

trait ExceptCompleteInstances {
  implicit def applicativeErrorComplete[F[_], G[_], R, E, A](
      implicit G: ApplicativeError[G, E],
      F: Monad[F],
      success: Complete[F, R, A],
      error: AsResponse.Error[E],
      lift: LiftHttp[F, G]
  ): ExceptComplete[F, R, E, G[A]] =
    fa => F.flatMap(lift(G.attempt(fa)))(_.fold(error(_).pure[F], success.complete))
}
trait ExceptPureCompletes {
  implicit def pureOptionComplete[F[_], R, A](
      implicit F: Applicative[F],
      success: Complete[F, R, A],
  ): ExceptComplete[F, R, None.type, Option[A]] =
    _.fold(Response(Status.NotFound).pure[F])(success.complete)

  implicit def pureEitherComplete[F[_], E, R, A](
      implicit F: Applicative[F],
      success: Complete[F, R, A],
      err: AsResponse.Error[E],
  ): ExceptComplete[F, R, E, Either[E, A]] =
    _.fold(err(_).pure[F], success.complete)
}
