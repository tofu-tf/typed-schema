package ru.tinkoff.tschema.finagle
import cats.data.{NonEmptyList, NonEmptyMap}
import com.twitter.finagle.{Service, http}
import scalaz.zio.{Exit, Runtime, ZIO}
import cats.instances.string._
import cats.syntax.semigroup._
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Future, Promise}
import scalaz.zio.Exit.Cause

object zioInstance {

  sealed trait Fail[+E]

  final case class Rejected(ms: NonEmptyMap[String, Rejection]) extends Fail[Nothing]
  final case class OtherFail[E](err: E)                         extends Fail[E]

  trait HasRequest[+R] {
    def request: http.Request
    def path: String
    def withPath(path: String): HasRequest[R]
    def modPath(f: String => String): HasRequest[R] = withPath(f(path))
    def embedded: R
  }

  type ZIOHttp[-R, +E, +A] = ZIO[HasRequest[R], Fail[E], A]
  type ZIORoute[+A]        = ZIOHttp[Any, Nothing, A]

  implicit val zioRouted: Routed[ZIORoute] =
    new Routed[ZIORoute] {
      def request: ZIORoute[http.Request]                                = ZIO.access(_.request)
      def path: ZIORoute[String]                                         = ZIO.access(_.path)
      def withPath[A](fa: ZIORoute[A], f: String => String): ZIORoute[A] = fa.provideSome(_.modPath(f))
      def reject[A](rejection: Rejection): ZIORoute[A] =
        ZIO.accessM(hasReq => ZIO.fail(Rejected(NonEmptyMap.one(hasReq.path, rejection))))
      def combineK[A](x: ZIORoute[A], y: ZIORoute[A]): ZIORoute[A] =
        x.catchSome { case Rejected(xrs) => y.catchSome { case Rejected(yrs) => ZIO.fail(Rejected(xrs |+| yrs)) } }
    }

  def zioRunnable[R, E <: Throwable](runtime: Runtime[R],
                                     rejectionHandler: Rejection.Handler): Runnable[ZIOHttp[R, E, ?], ZIO[R, E, ?]] =
    new Runnable[ZIOHttp[R, E, ?], ZIO[R, E, ?]] {
      def run(fresp: ZIOHttp[R, E, Response]): ZIO[R, E, Service[Request, Response]] =
        ZIO.succeedLazy { request =>
          {
            val promise = Promise[Response]
            runtime.unsafeRunAsync(
              fresp.provideSome[R](r => ZioRoutingEnv(request, request.path, r)).catchAll {
                case Rejected(rejections) => ZIO.succeed(rejectionHandler(rejections))
                case OtherFail(e)         => ZIO.fail(e)
              }
            ) {
              case Exit.Success(resp)  => promise.setValue(resp)
              case Exit.Failure(cause) => promise.raise(cause.squash)
            }
            promise
          }
        }
      def apply[A](fa: ZIO[R, E, A]): ZIOHttp[R, E, A] = fa.mapError(OtherFail(_)).provideSome(_.embedded)
    }

  final case class ZioRoutingEnv[+R](
      request: http.Request,
      path: String,
      embedded: R
  ) extends HasRequest[R] {
    def withPath(path: String): ZioRoutingEnv[R] = copy(path = path)
  }
}
