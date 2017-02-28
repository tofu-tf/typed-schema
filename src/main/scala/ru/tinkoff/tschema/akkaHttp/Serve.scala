package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.server._
import Directives._
import ru.tinkoff.tschema.typeDSL._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.coproduct.Prepend

import scala.language.higherKinds

trait Serve[T, P <: HList] extends ServePartial[T, P] {
  self ⇒
  type Input <: Coproduct
  type Output <: Coproduct

  def to[U]: Serve.Aux[U, P, Input, Output] = self.asInstanceOf[Serve.Aux[U, P, Input, Output]]
}

object Serve {
  type Aux[T, P <: HList, I <: Coproduct, O <: Coproduct] = Serve[T, P] {type Input = I; type Output = O}

  implicit def serveCons[start, end, P <: HList, startIn <: HList, endIn <: Coproduct, endOut <: Coproduct]
  (implicit
   start: ServePrefix.Aux[start, P, startIn],
   end: Aux[end, startIn, endIn, endOut]
  ): Aux[start :> end, P, endIn, endOut] = new Serve[start :> end, P] {
    type Input = endIn
    type Output = endOut

    def apply(f: Input ⇒ Route, provide: Provide[P]) = end(f, start.curry(provide))
  }

  implicit def serveSingle[x, P <: HList](implicit serve: ServeSingle[x, P]): Aux[x, P, FieldType[serve.Key, serve.Input] :+: CNil, FieldType[serve.Key, serve.Output] :+: CNil] =
    new Serve[x, P] {
      type Input = FieldType[serve.Key, serve.Input] :+: CNil
      type Output = FieldType[serve.Key, serve.Output] :+: CNil
      def apply(f: Input ⇒ Route, provide: Provide[P]): Route = serve(x ⇒ f(Inl[FieldType[serve.Key, serve.Input], CNil](field[serve.Key](x))), provide)
    }

  implicit def serveJoin[left, right, P <: HList, leftIn <: Coproduct, leftOut <: Coproduct, rightIn <: Coproduct, rightOut <: Coproduct]
  (implicit
   left: Aux[left, P, leftIn, leftOut],
   right: Aux[right, P, rightIn, rightOut],
   sumIn: Prepend[leftIn, rightIn],
   sumOut: Prepend[leftOut, rightOut]): Aux[left <|> right, P, sumIn.Out, sumOut.Out] = new Serve[left <|> right, P] {
    type Input = sumIn.Out
    type Output = sumOut.Out
    def apply(f: (Input) ⇒ Route, provide: Provide[P]): Route =
      left(x ⇒ f(sumIn(Left(x))), provide) ~ right(x ⇒ f(sumIn(Right(x))), provide)
  }

  implicit def metaLeftServe[left <: Meta, right, P <: HList](implicit right: Serve[right, P]) = right.to[left <|> right]
  implicit def metaRightServe[left, right <: Meta, P <: HList](implicit left: Serve[left, P]) = left.to[left <|> right]
}






