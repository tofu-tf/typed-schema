package ru.tinkoff.tschema
package finagle
import cats.syntax.flatMap._
import cats.{Applicative, Contravariant, Functor, Monad, MonoidK}
import com.twitter.finagle.http.{Response, Status}
import ru.tinkoff.tschema.Decompose
import ru.tinkoff.tschema.Decompose.{Cons, Last, NotFound}
import ru.tinkoff.tschema.finagle.util.message
import shapeless.Lazy
import cats.syntax.applicative._
import cats.syntax.apply._

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Could not complete ${A} knowing that result should be ${Out} using ${In} in ${F}. Make sure you have appropriate serializing instance and imported complete implementation from tethysIntances, circeInstances, etc."
)
trait CompleteIn[F[_], -In, Out, A] {
  def completeIn(a: A, in: In): F[Response]

  def as[Out1]: CompleteIn[F, In, Out1, A] = this.asInstanceOf[CompleteIn[F, In, Out1, A]]
}

object CompleteIn

trait Completing[F[_], R, A] extends CompleteIn[F, Any, R, A] {
  def complete(a: A): F[Response]

  def completeIn(a: A, in: Any): F[Response] = complete(a)

  override def as[R1]: Completing[F, R1, A] = this.asInstanceOf[Completing[F, R1, A]]
}

object Completing {
  implicit def contravariant[F[_], R]: Contravariant[Completing[F, R, *]] =
    new Contravariant[Completing[F, R, *]] {
      def contramap[A, B](fa: Completing[F, R, A])(f: B => A): Completing[F, R, B] = b => fa.complete(f(b))
    }
}