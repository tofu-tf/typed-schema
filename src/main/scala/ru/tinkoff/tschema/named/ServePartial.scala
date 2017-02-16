package ru.tinkoff.tschema.named

import akka.http.scaladsl.server.Route

trait ServePartial[T, Input, Output] {
  def handle(f: Input ⇒ Route): Route
}
