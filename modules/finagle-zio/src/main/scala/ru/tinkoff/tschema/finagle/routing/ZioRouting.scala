package ru.tinkoff.tschema.finagle.routing

import cats.Monad
import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.routing.ZioRouting.ZIOHttp
import ru.tinkoff.tschema.finagle.{ConvertService, LiftHttp, Rejection, Routed, RoutedPlus, Runnable}
import ru.tinkoff.tschema.utils.SubString
import zio.{Exit, Fiber, UIO, ZIO}

final case class ZioRouting[+R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
)

object ZioRouting extends ZioRoutedImpl {

  type ZIOHttp[-R, +E, +A] = ZIO[ZioRouting[R], Fail[E], A]

  implicit def zioRouted[R, E]: RoutedPlus[ZIOHttp[R, E, *]] with LiftHttp[ZIOHttp[R, E, *], ZIO[R, E, *]] =
    zioRoutedAny.asInstanceOf[ZioRoutedInstance[R, E]]

  def zioConvertService[R, E](f: Throwable => Fail[E]): ConvertService[ZIOHttp[R, E, *]] =
    new ConvertService[ZIOHttp[R, E, *]] {
      def convertService[A](svc: Service[http.Request, A]): ZIOHttp[R, E, A] =
        ZIO.accessM { r =>
          ZIO.effectAsyncInterrupt { cb =>
            val fut = svc(r.request).respond {
              case twitter.util.Return(a) => cb(ZIO.succeed(a))
              case twitter.util.Throw(ex) => cb(ZIO.fail(f(ex)))
            }

            Left(UIO(fut.raise(new InterruptedException)))
          }
        }
    }

  implicit def zioRunnable[R, E <: Throwable](
      implicit
      rejectionHandler: Rejection.Handler = Rejection.defaultHandler
  ): Runnable[ZIOHttp[R, E, *], ZIO[R, E, *]] =
    zioResponse => ZIO.runtime[R].flatMap(runtime => ZIO.effectTotal(execResponse(runtime, zioResponse, _)))

  private[this] def execResponse[R, E <: Throwable](
      runtime: zio.Runtime[R],
      zioResponse: ZIOHttp[R, E, Response],
      request: Request
  )(implicit handler: Rejection.Handler): Future[Response] = {
    val promise = Promise[Response]

    runtime.unsafeRunAsync(
      interruption
        .set(zioResponse, promise, runtime)
        .provideSome[R](r => ZioRouting(request, SubString(request.path), 0, r))
        .catchAll {
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
private[finagle] class ZioRoutedImpl {

  protected trait ZioRoutedInstance[R, E]
      extends RoutedPlus[ZIOHttp[R, E, *]] with LiftHttp[ZIOHttp[R, E, *], ZIO[R, E, *]] {
    private type F[a] = ZIOHttp[R, E, a]
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

    def apply[A](fa: ZIO[R, E, A]): F[A] = fa.mapError(Fail.Other(_)).provideSome(_.embedded)

    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.catchSome { case Fail.Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] = ZIO.fail(Fail.Rejected(map))
  }

  protected[this] object zioRoutedAny extends ZioRoutedInstance[Any, Nothing]

}
