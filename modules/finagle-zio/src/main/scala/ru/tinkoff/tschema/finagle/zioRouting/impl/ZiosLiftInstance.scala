package ru.tinkoff.tschema.finagle.zioRouting
package impl

import cats.evidence.As
import ru.tinkoff.tschema.finagle.LiftHttp
import ru.tinkoff.tschema.finagle.zioRouting.Fail
import zio.ZIO

private[zioRouting] class ZiosLiftInstance[R, R1, E, E1](implicit ev: E1 <:< E, evR: R As R1)
    extends LiftHttp[ZIOH[R, E, *], ZIO[R1, E1, *]] {
  private type F[a] = ZIOH[R, E, a]

  def apply[A](fa: ZIO[R1, E1, A]): ZIOH[R, E, A] = {
    type U[-x] = ZIOH[x, E, A]
    evR.substitute[U](fa.mapError(e => Fail.Other(e)))
  }
}
