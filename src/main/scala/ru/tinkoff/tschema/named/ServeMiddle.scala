package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Route
import shapeless.HList

trait ServeMiddle[x, P <: HList] extends ServePartial[x, P]{
  type Input = P
  def apply(f: P ⇒ Route, provide: Provide[P]): Route
}
