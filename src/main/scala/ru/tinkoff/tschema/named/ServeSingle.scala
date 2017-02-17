package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.serve.ServableSingle
import ru.tinkoff.tschema.typeDSL._
import shapeless.HList
import shapeless.ops.hlist._
import shapeless.ops.record.Values

trait ServeSingle[T] extends ServePartial[T] {
  type Input <: HList
  type Key
  def handle(f: (Input) ⇒ Route): Route

  def apply[SIn <: HList](servable: ServableSingle[SIn, Output])
                         (implicit values: Values.Aux[Input, SIn]): Route =
    handle(in ⇒ servable.route(values.apply(in)))
}

object ServeSingle {
  type Aux[T, I, O] = ServeSingle[T] {type Input = I; type Output = O}
  type TAux[T, I, O, Q] = ServeSingle[T] {type Input = I; type Output = O; type Key = Q}

  def apply[T] = new MkServe[T]

  def make[T](x: T) = new MkServe[T]

  class MkServe[T] {
    def apply[I <: HList, SI <: HList, O](servable: ServableSingle[SI, O])
                                         (implicit
                                          serve: Aux[T, I, O],
                                          values: Values.Aux[I, SI]): Route = serve(servable)
  }

  implicit def serveCons[start, end]
  (implicit
   start: ServePrefix[start],
   end: ServePostfix[end]) = new ServeSingle[start :> end] {
    type Input = start.Input
    type Output = end.Output
    type Key = start.Key
    def handle(f: Input => Route): Route = start.handle(in ⇒ end.transform(f(in)))
  }
}