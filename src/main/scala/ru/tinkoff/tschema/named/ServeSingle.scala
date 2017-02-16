package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.serve.ServableSingle
import ru.tinkoff.tschema.typeDSL._
import shapeless.HList
import shapeless.ops.hlist._
import shapeless.ops.record.Values

trait ServeSingle[T, In <: HList, Out] extends ServePartial[T, In, Out] {
  type Tag
  def handle(f: (In) ⇒ Route): Route

  def apply[SIn <: HList](servable: ServableSingle[SIn, Out])
                         (implicit values: Values.Aux[In, SIn]): Route =
    handle(in ⇒ servable.route(values.apply(in)))
}

object ServeSingle {
  def apply[T] = new MkServe[T]

  def make[T](x: T) = new MkServe[T]

  class MkServe[T] {
    def apply[I <: HList, SI <: HList, O](servable: ServableSingle[SI, O])
                                         (implicit
                                          serve: ServeSingle[T, I, O],
                                          values: Values.Aux[I, SI]): Route = serve(servable)
  }

  implicit def serveCons[start, end, input <: HList, output]
  (implicit
   start: ServePrefix[start, input],
   end: ServePostfix[end, output]) = new ServeSingle[start :> end, input, output] {
    type Tag = start.Tag
    def handle(f: input => Route): Route = start.handle(in ⇒ end.transform(f(in)))
  }
}