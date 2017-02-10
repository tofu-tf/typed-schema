package ru.tinkoff.tschema.serve

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.typeDSL._
import shapeless.HList

trait Servable[Input, Output] {
  def route(x: Input): Route
}

trait ToServable[x, I, O] extends (x ⇒ Servable[I, O]) {
  def apply(v1: x): Servable[I, O]
}

object ToServable {
  implicit def singleServable[x, Input <: HList, Out](implicit single: ToServableSingle[x, Input, Out]) = new ToServable[x, Input, Out] {
    def apply(x: x) = new Servable[Input, Out] {
      def route(y: Input) = single(x).route(y)
    }
  }

  implicit def joinServable[left, right, leftIn, rightIn, leftOut, rightOut]
  (implicit
   left: ToServable[left, leftIn, leftOut],
   right: ToServable[right, rightIn, rightOut]) = new ToServable[left <|> right, Either[leftIn, rightIn], Either[leftOut, rightOut]] {
    def apply(x: left <|> right) = new Servable[Either[leftIn, rightIn], Either[leftOut, rightOut]] {
      def route(y: Either[leftIn, rightIn]): Route = y match {
        case Left(l: leftIn@unchecked) ⇒ left(x.left).route(l)
        case Right(r: rightIn@unchecked) ⇒ right(x.right).route(r)
      }
    }
  }
}