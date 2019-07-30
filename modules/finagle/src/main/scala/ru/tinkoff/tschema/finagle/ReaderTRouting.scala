package ru.tinkoff.tschema.finagle

import cats.{Applicative, ApplicativeError, Monad}
import cats.data.ReaderT
import cats.effect.{Async, Effect, IO, Sync}
import cats.syntax.semigroup._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.effect.syntax.effect._
import cats.syntax.applicativeError._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.ReaderTRouting.ReaderHttp
import ru.tinkoff.tschema.utils.SubString

final case class ReaderTRouting[+R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
)

object ReaderTRouting extends EnvInstanceDecl {

  type ReaderHttp[F[_], R, A] = ReaderT[F, ReaderTRouting[R], A]

  implicit def zioRouted[F[_]: Async, R, E]: RoutedPlus[ReaderHttp[F, R, *]]
    with ConvertService[ReaderHttp[F, R, *]] with LiftHttp[ReaderHttp[F, R, *], ReaderT[F, R, *]] =
    new ReaderTRoutedConvert[F, R]

  implicit def envRunnable[F[_]: Effect, R](implicit rejectionHandler: Rejection.Handler = Rejection.defaultHandler)
    : Runnable[ReaderHttp[F, R, *], ReaderT[F, R, *]] =
    zioResponse => ReaderT(r => Sync[F].delay(execResponse(r, zioResponse, _)))

  private[this] def execResponse[F[_]: Effect, R](r: R, envResponse: ReaderHttp[F, R, Response], request: Request)(
      implicit handler: Rejection.Handler): Future[Response] = {
    val promise = Promise[Response]
    val routing = ReaderTRouting(request, SubString(request.path), 0, r)

    envResponse.run(routing).recover { case Rejected(rej) => handler(rej) }.runAsync {
      case Right(res) => IO(promise.setValue(res))
      case Left(ex) =>
        IO {
          val resp = Response(Status.InternalServerError)
          resp.setContentString(ex.getMessage)
          promise.setValue(resp)
        }
    }

    promise
  }
}

private[finagle] class EnvInstanceDecl {

  final case class Rejected(rej: Rejection) extends Throwable

  protected class ReaderTRoutedConvert[G[_]: Async, R]
      extends RoutedPlus[ReaderHttp[G, R, *]] with ConvertService[ReaderHttp[G, R, *]]
      with LiftHttp[ReaderHttp[G, R, *], ReaderT[G, R, *]] {
    private type F[a] = ReaderHttp[G, R, a]
    implicit private[this] val self: RoutedPlus[F] = this

    def matched: F[Int] = ReaderT(_.matched.pure[G])

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.local(_.copy(matched = m))

    def path: F[CharSequence]    = ReaderT(_.path.pure[G])
    def request: F[http.Request] = ReaderT(_.request.pure[G])
    def reject[A](rejection: Rejection): F[A] =
      Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

    def combineK[A](x: F[A], y: F[A]): F[A] =
      catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))

    def convertService[A](svc: Service[http.Request, A]): F[A] =
      ReaderT(r =>
        Async[G].async(cb =>
          svc(r.request).respond {
            case twitter.util.Return(a) => cb(Right(a))
            case twitter.util.Throw(ex) => cb(Left(ex))
        }))

    def apply[A](fa: ReaderT[G, R, A]): F[A] = ReaderT(routing => fa.run(routing.embedded))
  }

  @inline private[this] def catchRej[F[_]: Sync, R, A](z: ReaderHttp[F, R, A])(
      f: Rejection => ReaderHttp[F, R, A]): ReaderHttp[F, R, A] =
    z.recoverWith { case Rejected(xrs) => f(xrs) }

  @inline private[this] def throwRej[F[_]: Sync, R, A](map: Rejection): ReaderHttp[F, R, A] =
    Sync[ReaderHttp[F, R, *]].raiseError(Rejected(map))

}
