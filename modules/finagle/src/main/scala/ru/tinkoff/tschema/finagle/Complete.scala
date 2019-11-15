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

object CompleteIn extends CompositeCompleteInInstances with DefaultCompleteInInstances

trait Complete[F[_], R, A] extends CompleteIn[F, Any, R, A] {
  def complete(a: A): F[Response]

  def completeIn(a: A, in: Any): F[Response] = complete(a)

  override def as[R1]: Complete[F, R1, A] = this.asInstanceOf[Complete[F, R1, A]]
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
trait DefaultCompleteInInstances {
  final implicit def notFoundCompleteInstance[F[_]: Applicative, G[_]]: Complete[F, NotFound.type, NotFound.type] =
    message.emptyComplete(Status.NotFound)

  final implicit def notFoundCompleteFInstance[F[_]: Applicative, G[_]](
      implicit lift: LiftHttp[F, G]
  ): Complete[F, NotFound.type, G[NotFound.type]] =
    in => lift(in) *> Response(Status.NotFound).pure[F]

  final implicit def unitCompleteInstance[F[_]: Applicative, G[_]]: Complete[F, Unit, Unit] = message.emptyComplete()

  final implicit def unitCompleteFInstance[F[_]: Applicative, G[_]](
      implicit lift: LiftHttp[F, G]
  ): Complete[F, Unit, G[Unit]] =
    in => lift(in) *> Response(Status.Ok).pure[F]
}
