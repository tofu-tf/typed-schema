package ru.tinkoff.tschema.finagle.routing

import cats.Monad
import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.routing.IoRouting.IOHttp
import ru.tinkoff.tschema.finagle.{ConvertService, LiftHttp, Rejection, Routed, RoutedPlus, Runnable}
import ru.tinkoff.tschema.utils.SubString
import zio.{Exit, IO, ZIO}

final case class IoRouting(
    request: http.Request,
    path: CharSequence,
    matched: Int
)

object IoRouting extends IoRoutedImpl {

  type IOHttp[+E, +A] = ZIO[IoRouting, Fail[E], A]

  implicit def ioRouted[E]: RoutedPlus[IOHttp[E, *]] with LiftHttp[IOHttp[E, *], IO[E, *]] =
    ioRoutedAny.asInstanceOf[IoRoutedInstance[E]]

  def ioConvertService[E](f: Throwable => Fail[E]): ConvertService[IOHttp[E, *]] =
    new ConvertService[IOHttp[E, *]] {
      def convertService[A](svc: Service[http.Request, A]): IOHttp[E, A] =
        ZIO.accessM(r =>
          ZIO.effectAsync(cb =>
            svc(r.request).respond {
              case twitter.util.Return(a) => cb(ZIO.succeed(a))
              case twitter.util.Throw(ex) => cb(ZIO.fail(f(ex)))
          }))
    }

  implicit def ioRunnable[E <: Throwable](
      implicit
      rejectionHandler: Rejection.Handler = Rejection.defaultHandler): Runnable[IOHttp[E, *], IO[E, *]] =
    zioResponse => ZIO.runtime[Any].flatMap(runtime => ZIO.effectTotal(execResponse(runtime, zioResponse, _)))

  private[this] def execResponse[E <: Throwable](runtime: zio.Runtime[Any], zioResponse: IOHttp[E, Response], request: Request)(
      implicit handler: Rejection.Handler): Future[Response] = {
    val promise = Promise[Response]
    runtime.unsafeRunAsync(
      zioResponse.provide(IoRouting(request, SubString(request.path), 0)).catchAll {
        case Fail.Rejected(rejection) => ZIO.succeed(handler(rejection))
        case Fail.Other(e)            => ZIO.fail(e)
      }
    ) {
      case Exit.Success(resp) => promise.setValue(resp)
      case Exit.Failure(cause) =>
        val resp = Response(Status.InternalServerError)
        resp.setContentString(cause.squash.getMessage)
        promise.setValue(resp)
    }
    promise
  }
}
private[finagle] class IoRoutedImpl {

  protected trait IoRoutedInstance[E] extends RoutedPlus[IOHttp[E, *]] with LiftHttp[IOHttp[E, *], IO[E, *]] {
    private type F[a] = IOHttp[E, a]
    implicit private[this] val self: RoutedPlus[F] = this
    implicit private[this] val monad: Monad[F]     = zio.interop.catz.ioInstances

    def matched: F[Int] = ZIO.access(_.matched)

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.provideSome(_.copy(matched = m))

    def path: F[CharSequence]    = ZIO.access(_.path)
    def request: F[http.Request] = ZIO.access(_.request)
    def reject[A](rejection: Rejection): F[A] =
      Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

    def combineK[A](x: F[A], y: F[A]): F[A] =
      catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))

    def apply[A](fa: IO[E, A]): F[A] = fa.mapError(Fail.Other(_))

    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.catchSome { case Fail.Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] = ZIO.fail(Fail.Rejected(map))
  }

  protected[this] object ioRoutedAny extends IoRoutedInstance[Nothing]

}
