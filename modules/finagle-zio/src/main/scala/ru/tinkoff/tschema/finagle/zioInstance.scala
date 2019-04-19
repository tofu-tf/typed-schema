package ru.tinkoff.tschema.finagle
import cats.Monad
import cats.data.NonEmptyMap
import cats.instances.string._
import cats.syntax.semigroup._
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.utils.SubString
import scalaz.zio
import scalaz.zio.{Exit, Ref, ZIO}

object zioInstance {

  sealed trait Fail[+E]

  final case class Rejected(rej: Rejection) extends Fail[Nothing]
  final case class OtherFail[E](err: E)     extends Fail[E]

  final case class ZioRouting[+R](
      request: http.Request,
      path: CharSequence,
      matched: Ref[Int],
      embedded: R
  )

  type ZIOHttp[-R, +E, +A] = ZIO[ZioRouting[R], Fail[E], A]
  type ZIORoute[+A]        = ZIOHttp[Any, Nothing, A]

  def zioRouted[R, E]: Routed[ZIOHttp[R, E, ?]] =
    new Routed[ZIOHttp[R, E, ?]] {
      val FMonad: Monad[ZIOHttp[R, E, ?]] = zio.interop.catz.ioInstances

      def matched: ZIORoute[Int]                = ZIO.accessM(_.matched.get)
      def setMatched[A](m: Int): ZIORoute[Unit] = ZIO.accessM(_.matched.set(m))
      def path: ZIORoute[CharSequence]          = ZIO.access(_.path)
      def request: ZIORoute[http.Request]       = ZIO.access(_.request)
      def reject[A](rejection: Rejection): ZIOHttp[R, E, A] =
        unmatchedPath.flatMap(path => throwRej(rejection withPath path.toString))

      def combineK[A](x: ZIOHttp[R, E, A], y: ZIOHttp[R, E, A]): ZIOHttp[R, E, A] =
        matched >>= (m => catchRej(x)(xrs => setMatched(m) *> catchRej(y)(yrs => throwRej(xrs |+| yrs))))
    }

  def zioRunnable[R, E <: Throwable](
      rejectionHandler: Rejection.Handler = Rejection.defaultHandler): Runnable[ZIOHttp[R, E, ?], ZIO[R, E, ?]] =
    new Runnable[ZIOHttp[R, E, ?], ZIO[R, E, ?]] {
      def run(zioResponse: ZIOHttp[R, E, Response]): ZIO[R, E, Service[Request, Response]] =
        ZIO.runtime[R].flatMap(runtime => ZIO.effectTotal(execResponse(runtime, rejectionHandler, zioResponse, _)))

      def apply[A](fa: ZIO[R, E, A]): ZIOHttp[R, E, A] = fa.mapError(OtherFail(_)).provideSome(_.embedded)
    }

  @inline private[this] def catchRej[R, E, A](z: ZIOHttp[R, E, A])(f: Rejection => ZIOHttp[R, E, A]): ZIOHttp[R, E, A] =
    z.catchSome { case Rejected(xrs) => f(xrs) }

  @inline private[this] def throwRej[R, E, A](map: Rejection): ZIOHttp[R, E, A] = ZIO.fail(Rejected(map))

  @inline private[this] def execResponse[R, E <: Throwable](runtime: zio.Runtime[R],
                                                            handler: Rejection.Handler,
                                                            zioResponse: ZIOHttp[R, E, Response],
                                                            request: Request): Future[Response] = {
    val promise = Promise[Response]
    runtime.unsafeRunAsync(
      Ref
        .make(0)
        .flatMap(ref => zioResponse.provideSome[R](r => ZioRouting(request, SubString(request.path), ref, r)))
        .catchAll {
          case Rejected(rejection) => ZIO.succeed(handler(rejection))
          case OtherFail(e)        => ZIO.fail(e)
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
