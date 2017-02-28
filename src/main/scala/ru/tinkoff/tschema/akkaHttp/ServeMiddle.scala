package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.server.Route
import shapeless.HList

trait ServeMiddle[x, P <: HList, key] extends ServePartial[x, P]{
  type Input = P
  def apply(f: P ⇒ Route, provide: Provide[P]): Route
}
