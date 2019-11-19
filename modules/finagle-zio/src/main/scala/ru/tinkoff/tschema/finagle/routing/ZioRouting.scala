package ru.tinkoff.tschema.finagle.routing

import cats.Monad
import cats.syntax.semigroup._
import com.twitter.finagle.http
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.routing.ZioRouting.ZIOHttp
import ru.tinkoff.tschema.finagle.{ConvertService, LiftHttp, Rejection, Routed, RoutedPlus, RunHttp}
import ru.tinkoff.tschema.utils.SubString
import zio.{Exit, ZIO}

final case class ZioRouting[+R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
) extends ZioRoutingCommon

object ZioRouting extends ZioRoutedImpl {

  type ZIOHttp[-R, +E, +A] = ZIO[ZioRouting[R], Fail[E], A]

  implicit def zioRouted[R, E]: RoutedPlus[ZIOHttp[R, E, *]] =
    zioRoutedAny.asInstanceOf[ZioRoutedInstance[R, E]]

  implicit def zioLift[R, R1, E, E1](
      implicit eve: E1 <:< E,
      evr: R <:< R1
  ): LiftHttp[ZIOHttp[R, E, *], ZIO[R1, E1, *]] =
    zioLiftAny.asInstanceOf[ZioLiftInstance[R, R1, E, E1]]

  def zioConvertService[R, E](f: Throwable => Fail[E]): ConvertService[ZIOHttp[R, E, *]] =
    new ZIOConvertService[ZioRouting[R], Fail[E]](f)

  implicit def zioRunnable[R, E <: Throwable](
      implicit
      rejectionHandler: Rejection.Handler = Rejection.defaultHandler
  ): RunHttp[ZIOHttp[R, E, *], ZIO[R, E, *]] =
    zioResponse => ZIO.runtime[R].flatMap(runtime => ZIO.effectTotal(execResponse(runtime, zioResponse, _)))

  private[this] def execResponse[R, E <: Throwable](
      runtime: zio.Runtime[R],
      zioResponse: ZIOHttp[R, E, Response],
      request: Request
  )(implicit handler: Rejection.Handler): Future[Response] =
    execWithRuntime(runtime, request)(
      zioResponse
        .provideSome[R](r => ZioRouting(request, SubString(request.path), 0, r))
        .catchAll {
          case Fail.Rejected(rejection) => ZIO.succeed(handler(rejection))
          case Fail.Other(e)            => ZIO.fail(e)
        }
    )
}
private[finagle] class ZioRoutedImpl {

  protected trait ZioRoutedInstance[R, E] extends RoutedPlus[ZIOHttp[R, E, *]] {
    private type F[a] = ZIOHttp[R, E, a]
    implicit private[this] val self: RoutedPlus[F] = this
    implicit private[this] val monad: Monad[F]     = zio.interop.catz.monadErrorInstance

    def matched: F[Int] = ZIO.access(_.matched)

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.provideSome(_.copy(matched = m))

    def path: F[CharSequence]    = ZIO.access(_.path)
    def request: F[http.Request] = ZIO.access(_.request)
    def reject[A](rejection: Rejection): F[A] =
      Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

    def combineK[A](x: F[A], y: F[A]): F[A] =
      catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))
    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.catchSome { case Fail.Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] = ZIO.fail(Fail.Rejected(map))
  }

  protected class ZioLiftInstance[R, R1, E, E1](implicit eve: E1 <:< E, evr: R <:< R1)
      extends LiftHttp[ZIOHttp[R, E, *], ZIO[R1, E1, *]] {
    private type F[a] = ZIOHttp[R, E, a]
    def apply[A](fa: ZIO[R1, E1, A]): F[A] =
      fa.mapError(Fail.Other(_): Fail[E]).provideSome[ZioRouting[R]](_.embedded)
  }

  protected[this] object zioRoutedAny extends ZioRoutedInstance[Any, Nothing]
  protected[this] object zioLiftAny   extends ZioLiftInstance[Any, Any, Nothing, Nothing]

}
