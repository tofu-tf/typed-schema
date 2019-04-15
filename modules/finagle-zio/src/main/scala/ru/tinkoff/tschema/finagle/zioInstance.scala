package ru.tinkoff.tschema.finagle
import cats.Monad
import cats.data.NonEmptyMap
import cats.instances.string._
import cats.syntax.semigroup._
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import ru.tinkoff.tschema.finagle.Rejection.NotFound
import ru.tinkoff.tschema.utils.SubString
import scalaz.zio
import scalaz.zio.{Exit, Ref, ZIO}

import scala.collection.immutable.SortedMap

object zioInstance {

  sealed trait Fail[+E]

  final case class Rejected(ms: NonEmptyMap[String, Rejection]) extends Fail[Nothing]
  final case class OtherFail[E](err: E)                         extends Fail[E]

  final case class ZioRouting[+R](
      request: http.Request,
      path: CharSequence,
      matched: Ref[Int],
      embedded: R
  )

  type ZIOHttp[-R, +E, +A] = ZIO[ZioRouting[R], Fail[E], A]
  type ZIORoute[+A]        = ZIOHttp[Any, Nothing, A]

  val unmatched: ZIORoute[String] = ZIO.accessM(r => r.matched.get.map(r.request.path.substring))

  implicit val zioRouted: Routed[ZIORoute] =
    new Routed[ZIORoute] {
      val FMonad: Monad[ZIORoute] = zio.interop.catz.ioInstances

      def matched: ZIORoute[Int]                = ZIO.accessM(_.matched.get)
      def setMatched[A](m: Int): ZIORoute[Unit] = ZIO.accessM(_.matched.set(m))
      def path: ZIORoute[CharSequence]          = ZIO.access(_.path)
      def request: ZIORoute[http.Request]       = ZIO.access(_.request)
      def reject[A](rejection: Rejection): ZIORoute[A] =
        unmatched.flatMap(path => throwRej(NonEmptyMap.one(path, rejection)))

      def rejectMany[A](rejections: Rejection*): ZIORoute[A] = unmatched.flatMap { path =>
        throwRej(NonEmptyMap.fromMap(SortedMap(rejections.map(path -> _): _*)).getOrElse(NonEmptyMap.one(path, NotFound)))
      }

      def combineK[A](x: ZIORoute[A], y: ZIORoute[A]): ZIORoute[A] =
        matched >>= (m => catchRej(x)(xrs => setMatched(m) *> catchRej(y)(yrs => throwRej(xrs |+| yrs))))
    }

  def zioRunnable[R, E <: Throwable](
      rejectionHandler: Rejection.Handler = Rejection.defaultHandler): Runnable[ZIOHttp[R, E, ?], ZIO[R, E, ?]] =
    new Runnable[ZIOHttp[R, E, ?], ZIO[R, E, ?]] {
      def run(zioResponse: ZIOHttp[R, E, Response]): ZIO[R, E, Service[Request, Response]] =
        for {
          ref     <- Ref.make(0)
          runtime <- ZIO.runtime[R]
        } yield execResponse(ref, runtime, rejectionHandler, zioResponse, _)

      def apply[A](fa: ZIO[R, E, A]): ZIOHttp[R, E, A] = fa.mapError(OtherFail(_)).provideSome(_.embedded)
    }

  @inline private[this] def catchRej[A](z: ZIORoute[A])(f: NonEmptyMap[String, Rejection] => ZIORoute[A]): ZIORoute[A] =
    z.catchSome { case Rejected(xrs) => f(xrs) }

  @inline private[this] def throwRej[A](map: NonEmptyMap[String, Rejection]): ZIORoute[A] =
    ZIO.fail(Rejected(map))

  @inline private[this] def execResponse[R, E <: Throwable](matchRef: Ref[Int],
                                                            runtime: zio.Runtime[R],
                                                            handler: Rejection.Handler,
                                                            zioResponse: ZIOHttp[R, E, Response],
                                                            request: Request): Future[Response] = {
    val promise = Promise[Response]
    runtime.unsafeRunAsync(
      zioResponse.provideSome[R](r => ZioRouting(request, SubString(request.path), matchRef, r)).catchAll {
        case Rejected(rejections) => ZIO.succeed(handler(rejections))
        case OtherFail(e)         => ZIO.fail(e)
      }
    ) {
      case Exit.Success(resp)  => promise.setValue(resp)
      case Exit.Failure(cause) => promise.raise(cause.squash)
    }
    promise
  }
}
