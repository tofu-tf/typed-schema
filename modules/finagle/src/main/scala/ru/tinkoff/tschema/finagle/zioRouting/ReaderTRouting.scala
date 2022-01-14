package ru.tinkoff.tschema.finagle.zioRouting

import cats.data.ReaderT
import cats.effect.syntax.effect._
import cats.effect.{Async, Effect, IO, Sync}
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.Rejection.{OptRecover, Recover}
import ru.tinkoff.tschema.finagle.zioRouting.ReaderTRouting.ReaderHttp
import ru.tinkoff.tschema.finagle.{ConvertService, LiftHttp, Rejection, Routed, RoutedPlus, RunHttp}
import ru.tinkoff.tschema.utils.SubString

final case class ReaderTRouting[+R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
)

object ReaderTRouting extends ReaderTInstanceDecl {

  type ReaderHttp[F[_], R, A] = ReaderT[F, ReaderTRouting[R], A]

  implicit def readerTRouted[F[_]: Async, R]: RoutedPlus[ReaderHttp[F, R, *]]
    with ConvertService[ReaderHttp[F, R, *]] with LiftHttp[ReaderHttp[F, R, *], ReaderT[F, R, *]] =
    new ReaderTRoutedConvert[F, R]

  implicit def envRunnable[F[_]: Effect, R](implicit
      optRecover: OptRecover[ReaderHttp[F, R, *]] = OptRecover.default[ReaderHttp[F, R, *]]
  ): RunHttp[ReaderHttp[F, R, *], ReaderT[F, R, *]] = {
    implicit val rec: Recover[ReaderHttp[F, R, *]] = optRecover.orDefault
    response => ReaderT(r => Sync[F].delay(execResponse(r, response, _)))
  }

  private[this] def execResponse[F[_]: Effect, R](r: R, envResponse: ReaderHttp[F, R, Response], request: Request)(
      implicit recover: Recover[ReaderHttp[F, R, *]]
  ): Future[Response] = {
    val promise = Promise[Response]()
    val routing = ReaderTRouting(request, SubString(request.path), 0, r)

    envResponse.recoverWith { case Rejected(rej) => recover(rej) }.run(routing).runAsync {
      case Right(res) => IO(promise.setValue(res))
      case Left(ex)   =>
        IO {
          val resp    = Response(Status.InternalServerError)
          val message = Option(ex.getLocalizedMessage).getOrElse(ex.toString)
          resp.setContentString(message)
          promise.setValue(resp)
        }
    }

    promise
  }
}

private[finagle] class ReaderTInstanceDecl {
  private def cachedMonadInstance[G[_]: Async, R] = Async[ReaderHttp[G, R, *]]

  protected class ReaderTRoutedConvert[G[_]: Async, R]
      extends RoutedPlus[ReaderHttp[G, R, *]] with ConvertService[ReaderHttp[G, R, *]]
      with LiftHttp[ReaderHttp[G, R, *], ReaderT[G, R, *]] {
    private type F[a] = ReaderHttp[G, R, a]
    implicit private[this] val self: RoutedPlus[F] = this
    implicit private[this] val cached: Async[F]    = cachedMonadInstance

    def matched: F[Int] = ReaderT(_.matched.pure[G])

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.local(_.copy(matched = m))

    def path: F[CharSequence]                 = ReaderT(_.path.pure[G])
    def request: F[http.Request]              = ReaderT(_.request.pure[G])
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
          }
        )
      )

    def apply[A](fa: ReaderT[G, R, A]): F[A] = ReaderT(routing => fa.run(routing.embedded))

    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.recoverWith { case Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] =
      cached.raiseError(Rejected(map))
  }

}
