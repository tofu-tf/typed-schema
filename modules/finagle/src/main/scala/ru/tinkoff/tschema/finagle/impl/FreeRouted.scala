package ru.tinkoff.tschema.finagle
package impl

import cats.Monad
import cats.free.Free
import cats.free.Free.liftF
import com.twitter.finagle.http.Request
import ru.tinkoff.tschema.utils.functionK

private[finagle] class FreeRouted[F[_]](implicit F: Routed[F]) extends Routed[Free[F, ?]] {
  implicit val FMonad: Monad[Free[F, ?]]  = implicitly
  private implicit val lowMonad: Monad[F] = F.FMonad

  def request: Free[F, Request]                          = liftF(F.request)
  def path: Free[F, CharSequence]                        = liftF(F.path)
  def matched: Free[F, Int]                              = liftF(F.matched)
  def withMatched[A](m: Int, fa: Free[F, A]): Free[F, A] = fa.mapK(functionK[F](F.withMatched(m, _)))
  def reject[A](rejection: Rejection): Free[F, A]        = liftF(F.reject(rejection))
}
