package ru.tinkoff.tschema.serve

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server._
import Directives._
import ru.tinkoff.tschema.typeDSL._
import shapeless.ops.hlist._
import shapeless.{HList, HNil}

trait ServeSingle[T, In <: HList, Out] extends ServePartial[T, In] {

  def handle(f: (In) ⇒ Route): Route
}

object ServeSingle {
  def apply[T] = new MkServe[T]

  def make[T](x: T) = new MkServe[T]

  class MkServe[T] {
    def apply[I <: HList, O](servable: ServableSingle[I, O])(implicit serve: ServeSingle[T, I, O]): Route = serve(servable)
  }

  implicit def servePost[x]
  (implicit marshaller: ToResponseMarshaller[x]): ServeSingle[Post[x], HNil, x] =
    f => post(f(HNil))

  implicit def serveGet[x]
  (implicit marshaller: ToResponseMarshaller[x]): ServeSingle[Get[x], HNil, x] =
    f => get(f(HNil))

  implicit def serveCons[start, end, startInput <: HList, endInput <: HList, endOut]
  (implicit
   start: ServePrefix[start, startInput],
   end: ServeSingle[end, endInput, endOut],
   prepend: Prepend[startInput, endInput]): ServeSingle[start :> end, prepend.Out, endOut] =
    f => start.handle(startIn ⇒ end.handle { endIn ⇒ f(prepend(startIn, endIn)) })
}