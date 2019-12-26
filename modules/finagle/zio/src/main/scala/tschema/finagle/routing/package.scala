package tschema.finagle
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.{Future, Promise}
import tschema.utils.SubString
import zio.{Exit, Fiber, Runtime, ZIO}

package object routing {
  private[routing] def execWithRuntime[R, E <: Throwable](runtime: Runtime[R], req: Request)(
      zio: ZIO[R, E, Response]
  ): Future[Response] = {
    val promise = Promise[Response]

    runtime.unsafeRunAsync(setInterruption(zio, promise, runtime)) {
      case Exit.Success(resp) => promise.setValue(resp)
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
}
