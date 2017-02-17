package ru.tinkoff.tschema.named

import akka.http.scaladsl.server.Route

trait ServePartial[T] {
  type Input
  type Output
  def handle(f: Input ⇒ Route): Route
}
