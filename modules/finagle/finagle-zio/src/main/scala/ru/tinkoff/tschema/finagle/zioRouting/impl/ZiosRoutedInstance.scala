package ru.tinkoff.tschema.finagle.zioRouting
package impl

import cats.Monad
import com.twitter.finagle.http
import ru.tinkoff.tschema.finagle.zioRouting.Fail
import ru.tinkoff.tschema.finagle.{Rejection, Routed, RoutedPlus}
import zio.ZIO
import cats.syntax.monoid._

private[zioRouting] class ZiosRoutedInstance[R, E] extends RoutedPlus[ZIOH[R, E, *]] {
  private type F[a] = ZIOH[R, E, a]
  implicit private[this] val self: RoutedPlus[F] = this
  implicit private[this] val monad: Monad[F]     = zio.interop.catz.monadErrorInstance

  def matched: F[Int] = ZIO.access(_.get.matched)

  def withMatched[A](m: Int, fa: F[A]): F[A] =
    fa.provideSome(r => r add r.get.copy(matched = m))

  def path: F[CharSequence]                 = ZIO.access(_.get.path)
  def request: F[http.Request]              = ZIO.access(_.get.request)
  def reject[A](rejection: Rejection): F[A] =
    Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

  def combineK[A](x: F[A], y: F[A]): F[A] =
    catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))

  @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
    z.catchSome { case Fail.Rejected(xrs) => f(xrs) }

  @inline private[this] def throwRej[A](map: Rejection): F[A]                = ZIO.fail(Fail.Rejected(map))
}
