package ru.tinkoff.tschema.named

import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.serve.ServableSingle
import shapeless.HList
import shapeless.ops.record.Values

trait ServePartial[T, Input <: HList, Output] {
  def handle(f: Input ⇒ Route): Route

  def apply[SIn <: HList](servable: ServableSingle[SIn, Output])
                         (implicit values: Values.Aux[Input, SIn]): Route =
    handle(in ⇒ servable.route(values.apply(in)))
}
