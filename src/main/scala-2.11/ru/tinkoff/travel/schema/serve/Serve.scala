package ru.tinkoff.travel.schema.serve

import akka.http.scaladsl.server._
import Directives._
import ru.tinkoff.travel.schema.typeDSL._
import shapeless.ops.hlist._
import shapeless.{HList, DepFn2}

import scala.language.higherKinds

trait ServePartial[T, Input <: HList] {
  def handle(f: Input ⇒ Route): Route

  def apply(servable: ServableSingle[Input, _]): Route = handle(servable.route)
}

trait Serve[T, Input, Output] {
  self ⇒

  def handle(f: Input ⇒ Route): Route

  def apply(servable: Servable[Input, Output]) = handle(servable.route)

  def to[U] = new Serve[U, Input, Output] {
    override def handle(f: Input ⇒ Route) = self.handle(f)
  }
}

trait LowPriorityServe {
  implicit def serveCons[start, end, startIn <: HList, endIn, endOut, in]
  (implicit
   start: ServePrefix[start, startIn],
   end: Serve[end, endIn, endOut],
   distribute: Distribute[startIn, endIn, in]) = new Serve[start :> end, in, endOut] {
    def handle(f: (in) => Route): Route = start.handle { startIn ⇒ end.handle { endIn ⇒ f(distribute(startIn, endIn)) } }
  }
}

object Serve extends LowPriorityServe {
  def apply[T] = new MkServe[T]
  def make[T](x: T) = new MkServe[T]

  class MkServe[T] {
    def apply[In, Out, S](servable: S)
                         (implicit serve: Serve[T, In, Out],
                          convert: ToServable[S, In, Out]) =
      serve.handle(servable.route)
  }

  implicit def serveSingle[x, I <: HList, O](implicit serve: ServeSingle[x, I, O]) = new Serve[x, I, O] {
    def handle(f: (I) => Route): Route = serve.handle(f)
  }

  implicit def serveJoin[left, right, leftIn, leftOut, rightIn, rightOut]
  (implicit
   left: Serve[left, leftIn, leftOut],
   right: Serve[right, rightIn, rightOut]) =
    new Serve[<|>[left, right], Either[leftIn, rightIn], Either[leftOut, rightOut]] {
      def handle(f: (Either[leftIn, rightIn]) => Route): Route =
        left.handle(x ⇒ f(Left(x))) ~ right.handle(x ⇒ f(Right(x)))
    }

  implicit def metaLeftServe[left <: Meta, right, I, O](implicit right: Serve[right, I, O]): Serve[left <|> right, I, O] = right.to[left <|> right]
  implicit def metaRightServe[left, right <: Meta, I, O](implicit left: Serve[left, I, O]): Serve[left <|> right, I, O] = left.to[left <|> right]
}

/**
  * Widened version of Prepend, that could prepend single HList to all elements, connected by `Either`
  */
trait Distribute[list <: HList, x, O] extends DepFn2[list, x] {
  def apply(t: list, u: x): O

  override type Out = O
}

object Distribute {
  def apply[list <: HList, x, O](implicit dist: Distribute[list, x, O]) = dist

  implicit def distributeSingle[list <: HList, x <: HList]
  (implicit prepend: Prepend[list, x]) = new Distribute[list, x, prepend.Out] {
    def apply(list: list, x: x) = prepend(list, x)
  }

  implicit def distributeEither[list <: HList, left, right, leftOut, rightOut]
  (implicit
   left: Distribute[list, left, leftOut],
   right: Distribute[list, right, rightOut]) = new Distribute[list, Either[left, right], Either[leftOut, rightOut]] {
    def apply(list: list, x: Either[left, right]): Either[leftOut, rightOut] = x match {
      case Left(l) ⇒ Left(left(list, l))
      case Right(r) ⇒ Right(right(list, r))
    }
  }
}





