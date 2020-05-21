package ru.tinkoff.tschema.finagle
import com.twitter.finagle.http.{Response, Status}
import com.twitter.util.{Future, Promise}
import zio.{Exit, Fiber, Has, Runtime, ZIO}

package object zioRouting {
  type NoError <: Nothing
  type None >: Any

  type ZRouting   = ZioRouting[Any]
  type HasRouting = Has[ZRouting]

  type UIOHttp[+A]         = ZIO[ZioRouting[None], Fail[NoError], A]
  type IOHttp[+E, +A]      = ZIO[ZioRouting[None], Fail[E], A]
  type TaskHttp[+A]        = ZIO[ZioRouting[None], Fail[Throwable], A]
  type URIOHttp[-R, +A]    = ZIO[ZioRouting[R], Fail[NoError], A]
  type RIOHttp[-R, +A]     = ZIO[ZioRouting[R], Fail[Throwable], A]
  type ZIOHttp[-R, +E, +A] = ZIO[ZioRouting[R], Fail[E], A]

  type UIOH[+A]         = ZIO[HasRouting, Fail[NoError], A]
  type IOH[+E, +A]      = ZIO[HasRouting, Fail[E], A]
  type TaskH[+A]        = ZIO[HasRouting, Fail[Throwable], A]
  type URIOH[-R, +A]    = ZIO[HasRouting with R, Fail[NoError], A]
  type RIOH[-R, +A]     = ZIO[HasRouting with R, Fail[Throwable], A]
  type ZIOH[-R, +E, +A] = ZIO[HasRouting with R, Fail[E], A]

  private[zioRouting] def execWithRuntime[R, E <: Throwable](runtime: Runtime[R])(
      zio: ZIO[R, E, Response]
  ): Future[Response] = {
    val promise = Promise[Response]

    runtime.unsafeRunAsync(setInterruption(zio, promise, runtime)) {
      case Exit.Success(resp)  => promise.setValue(resp)
      case Exit.Failure(cause) =>
        val resp  = Response(Status.InternalServerError)
        val error = cause.squash
        resp.setContentString(Option(error.getLocalizedMessage).getOrElse(error.toString))
        promise.setValue(resp)
    }

    promise
  }

  private def setInterruption[R, E, A, X](zio: ZIO[R, E, A], promise: Promise[X], rt: Runtime[Any]): ZIO[R, E, A] = {
    def setInterrupt(fiber: Fiber[Any, Any]) =
      ZIO.effectTotal(promise.setInterruptHandler {
        case _ => rt.unsafeRunAsync(fiber.interrupt)(_ => ())
      })

    zio.fork.tap(setInterrupt) >>= (_.join)
  }

  private[zioRouting] def execResponse[R, R1, E <: Throwable](
      runtime: zio.Runtime[R],
      zioResponse: ZIO[R1, E, Response],
      f: R => R1
  ): Future[Response] =
    zioRouting.execWithRuntime(runtime)(
      zioResponse.provideSome[R](f)
    )
}
