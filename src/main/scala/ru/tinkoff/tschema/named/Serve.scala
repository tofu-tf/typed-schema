package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import Directives._
import ru.tinkoff.tschema.serve.ToServable
import ru.tinkoff.tschema.typeDSL._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.coproduct.Prepend

import scala.language.higherKinds

trait Serve[T, I, O] extends ServePartial[T, I, O] {
  self ⇒
  type Input = I
  type Output = O

  def handle(f: Input ⇒ Route): Route

  def to[U] = new Serve[U, Input, Output] {
    override def handle(f: Input ⇒ Route): Route = self.handle(f)
  }

  def apply[Impl](impl: Impl)(implicit routable: Routable[Input, Impl]): Route =
    handle(in ⇒ routable.routeWith(in, impl))
}

object Serve {
  def apply[T] = new MkServe[T]
  def make[T](x: T) = new MkServe[T]

  class MkServe[T] {
    def apply[In <: HList, Out, Impl](impl: Impl)
                                  (implicit serve: Serve[T, In, Out],
                                   convert: Routable.Aux[In, Impl, Out]) =
      serve.handle(x ⇒ convert.routeWith(x, impl))
  }

  implicit def serveCons[start, end, startIn <: HList, endIn <: Coproduct, endOut, in <: Coproduct]
  (implicit
   start: ServePrefix[start, startIn],
   end: Serve[end, endIn, endOut],
   distribute: Distribute.Aux[startIn, endIn, in]): Serve[start :> end, in, endOut] =
    f => start.handle { startIn ⇒ end.handle { endIn ⇒ f(distribute(startIn, endIn)) } }

  implicit def serveSingle[x, I <: HList, O](implicit serve: ServeSingle[x, I, O]): Serve[x, FieldType[serve.Tag, I] :+: CNil, O :+: CNil] =
    new Serve[x, FieldType[serve.Tag, I] :+: CNil, O :+: CNil] {
      def handle(f: (FieldType[serve.Tag, I] :+: CNil) ⇒ Route): Route = serve.handle(x ⇒ f(Inl[FieldType[serve.Tag, I], CNil](field[serve.Tag](x))))
    }

  implicit def serveJoin[left, right, leftIn <: Coproduct, leftOut <: Coproduct, rightIn <: Coproduct, rightOut <: Coproduct]
  (implicit
   left: Serve[left, leftIn, leftOut],
   right: Serve[right, rightIn, rightOut],
   sumIn: Prepend[leftIn, rightIn],
   sumOut: Prepend[leftOut, rightOut]): Serve[left <|> right, sumIn.Out, sumOut.Out] =
    new Serve[left <|> right, sumIn.Out, sumOut.Out] {
      def handle(f: (Input) ⇒ Route): Route =
        left.handle(x ⇒ f(sumIn(Left(x)))) ~ right.handle(x ⇒ f(sumIn(Right(x))))
    }

  implicit def metaLeftServe[left <: Meta, right, I <: HList, O](implicit right: Serve[right, I, O]): Serve[left <|> right, I, O] = right.to[left <|> right]
  implicit def metaRightServe[left, right <: Meta, I <: HList, O](implicit left: Serve[left, I, O]): Serve[left <|> right, I, O] = left.to[left <|> right]
}






