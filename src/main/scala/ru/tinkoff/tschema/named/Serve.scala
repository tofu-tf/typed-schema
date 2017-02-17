package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import Directives._
import ru.tinkoff.tschema.serve.ToServable
import ru.tinkoff.tschema.typeDSL._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.coproduct.Prepend

import scala.language.higherKinds

trait Serve[T] extends ServePartial[T] {
  self ⇒
  type Input <: Coproduct
  type Output <: Coproduct

  def handle(f: Input ⇒ Route): Route

  def to[U]: Serve.Aux[U, Input, Output] = new Serve[U] {
    type Input = self.Input
    type Output = self.Output
    override def handle(f: Input ⇒ Route): Route = self.handle(f)
  }

  def apply[Impl](impl: Impl)(implicit routable: Routable[Input, Impl]): Route =
    handle(in ⇒ routable.routeWith(in, impl))
}

object Serve {
  type Aux[T, I <: Coproduct, O <: Coproduct] = Serve[T] {type Input = I; type Output = O}

  def apply[T] = new MkServe[T]
  def make[T](x: T) = new MkServe[T]

  class MkServe[T] {
    def apply[In <: Coproduct, Out <: Coproduct, Impl](impl: Impl)
                                                      (implicit serve: Aux[T, In, Out],
                                                       convert: Routable.Aux[In, Impl, Out]) =
      serve.handle(x ⇒ convert.routeWith(x, impl))
  }

  implicit def serveCons[start, end, startIn <: HList, endIn <: Coproduct, endOut <: Coproduct]
  (implicit
   start: ServePrefix.Aux[start, startIn],
   end: Aux[end, endIn, endOut],
   in: DistributeLab[startIn, endIn]): Aux[start :> end, in.Out, endOut] = new Serve[start :> end] {
    type Input = in.Out
    type Output = endOut

    def handle(f: Input ⇒ Route) = start.handle { startIn ⇒ end.handle { endIn ⇒ f(in(startIn, endIn)) } }
  }

  implicit def serveSingle[x](implicit serve: ServeSingle[x]): Aux[x, FieldType[serve.Key, serve.Input] :+: CNil, FieldType[serve.Key, serve.Output] :+: CNil] =
    new Serve[x] {
      type Input = FieldType[serve.Key, serve.Input] :+: CNil
      type Output = FieldType[serve.Key, serve.Output] :+: CNil
      def handle(f: Input ⇒ Route): Route = serve.handle(x ⇒ f(Inl[FieldType[serve.Key, serve.Input], CNil](field[serve.Key](x))))
    }

  implicit def serveJoin[left, right, leftIn <: Coproduct, leftOut <: Coproduct, rightIn <: Coproduct, rightOut <: Coproduct]
  (implicit
   left: Aux[left, leftIn, leftOut],
   right: Aux[right, rightIn, rightOut],
   sumIn: Prepend[leftIn, rightIn],
   sumOut: Prepend[leftOut, rightOut]): Aux[left <|> right, sumIn.Out, sumOut.Out] = new Serve[left <|> right] {
    type Input = sumIn.Out
    type Output = sumOut.Out
    def handle(f: (Input) ⇒ Route): Route =
      left.handle(x ⇒ f(sumIn(Left(x)))) ~ right.handle(x ⇒ f(sumIn(Right(x))))
  }

  implicit def metaLeftServe[left <: Meta, right](implicit right: Serve[right]) = right.to[left <|> right]
  implicit def metaRightServe[left, right <: Meta](implicit left: Serve[left]) = left.to[left <|> right]
}






