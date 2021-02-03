package ru.tinkoff.tschema.finagle.zioRouting
package impl

import com.twitter
import com.twitter.finagle.Service
import com.twitter.finagle.http.Request
import ru.tinkoff.tschema.finagle.ConvertService
import zio.{UIO, ZIO}

private[finagle] class ZIOConvertService[R, E] extends ConvertService[ZIOHttp[R, E, *]] {
  def convertService[A](svc: Service[Request, A]): ZIOHttp[R, E, A] =
    ZIO.accessM { r =>
      ZIO.effectAsyncInterrupt[ZioRouting[R], Fail[E], A] { cb =>
        val fut = svc(r.request).respond {
          case twitter.util.Return(a) => cb(ZIO.succeed(a))
          case twitter.util.Throw(ex) => cb(ZIO.die(ex))
        }

        Left(UIO(fut.raise(new InterruptedException)))
      }
    }
}
