package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Route

trait Routable[In, Impl] {
  def routeWith(in: In, impl: Impl): Route
}
