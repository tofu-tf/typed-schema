package ru.tinkoff.tschema.finagle.routing
import com.twitter.util.Promise
import zio.{Fiber, Runtime, ZIO}

private[tschema] object interruption {
  def set[R, E, A, X](zio: ZIO[R, E, A], promise: Promise[X], rt: Runtime[Any]): ZIO[R, E, A] = {
    def setInterrupt(fiber: Fiber[Any, Any]) =
      ZIO.effectTotal(promise.setInterruptHandler {
        case _ => rt.unsafeRunAsync(fiber.interrupt)(_ => ())
      })

    zio.fork.tap(setInterrupt) >>= (_.join)
  }
}
