package ru.tinkoff.tschema
package finagle
import cats.syntax.flatMap._
import cats.{Applicative, Contravariant, Functor, Monad, MonoidK}
import com.twitter.finagle.http.{Response, Status}
import ru.tinkoff.tschema.Decompose
import ru.tinkoff.tschema.Decompose.{Cons, Last}
import ru.tinkoff.tschema.finagle.util.message
import shapeless.Lazy

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Could not complete ${A} knowing that result should be ${Out} using ${In} in ${F}. Make sure you have appropriate serializing instance and imported complete implementation from tethysIntances, circeInstances, etc.")
trait CompleteIn[F[_], -In, Out, A] {
  def completeIn(a: A, in: In): F[Response]
}

object CompleteIn extends CompositeCompleteInInstances

trait Complete[F[_], R, A] extends CompleteIn[F, Any, R, A] {
  def complete(a: A): F[Response]

  def completeIn(a: A, in: Any): F[Response] = complete(a)
}

object Complete {
  implicit def contravariant[F[_], R]: Contravariant[Complete[F, R, *]] =
    new Contravariant[Complete[F, R, *]] {
      def contramap[A, B](fa: Complete[F, R, A])(f: B => A): Complete[F, R, B] = b => fa.complete(f(b))
    }
}

trait CompositeCompleteInInstances {
  implicit def compositePureInstance[F[_], In, A, D](
      implicit
      decompose: Decompose.Aux[A, D],
      composite: Lazy[CompositeComplete[F, In, A, D]]
  ): CompleteIn[F, In, Composite[A], A] =
    (ga, in) => composite.value.complete(ga, decompose.self, in)

  implicit def compositeLiftInstance[F[_]: Monad, G[_], In, A, D](
      implicit
      decompose: Decompose.Aux[A, D],
      composite: Lazy[CompositeComplete[F, In, A, D]],
      lift: LiftHttp[F, G]
  ): CompleteIn[F, In, Composite[A], G[A]] =
    (ga, in) => lift(ga).flatMap(composite.value.complete(_, decompose.self, in))
}

trait CompositeComplete[F[_], In, A, D] {
  def complete(a: A, d: D, in: In): F[Response]
}

object CompositeComplete {
  implicit def lastComplete[F[_]: MonoidK, G[_], In, A]: CompositeComplete[F, In, A, Last[A]] =
    (_, _, _) => MonoidK[F].empty[Response]

  implicit def consComplete[F[_], In, A, H, T](
      implicit
      head: CompleteIn[F, In, H, H],
      tail: CompositeComplete[F, In, A, T]
  ): CompositeComplete[F, In, A, Cons[A, H, T]] =
    (a, d, in) => d.tryHead(a).fold(tail.complete(a, d.next, in))(head.completeIn(_, in))
}


