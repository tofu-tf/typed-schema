package ru.tinkoff.tschema.serve
import akka.http.scaladsl.server.Route
import shapeless.HList

trait ServePartial[T, Input <: HList] {
  def handle(f: Input ⇒ Route): Route

  def apply(servable: ServableSingle[Input, _]): Route = handle(servable.route)
}

