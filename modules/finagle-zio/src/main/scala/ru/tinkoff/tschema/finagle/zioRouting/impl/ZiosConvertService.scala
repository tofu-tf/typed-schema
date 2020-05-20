package ru.tinkoff.tschema.finagle.zioRouting
package impl

import com.twitter
import com.twitter.finagle.Service
import com.twitter.finagle.http.Request
import ru.tinkoff.tschema.finagle.ConvertService
import zio.{UIO, ZIO}

private[finagle] class ZiosConvertService[R, E] extends ConvertService[ZIOH[R, E, *]] {
  def convertService[A](svc: Service[Request, A]): ZIOH[R, E, A] =
    ZIO.accessM { r =>
      ZIO.effectAsyncInterrupt[R with HasRouting, Fail[E], A] { cb =>
        val fut = svc(r.get.request).respond {
          case twitter.util.Return(a) => cb(ZIO.succeed(a))
          case twitter.util.Throw(ex) => cb(ZIO.die(ex))
        }

        Left(UIO(fut.raise(new InterruptedException)))
      }
    }
}
