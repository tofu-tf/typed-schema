package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Directives.{get, post, put, delete, head, options, patch}
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.typeDSL.{Get, Post, Put, Delete, Head, Options}

trait ServePostfix[T, Out] extends ServePartial[T, Unit, Out] {
  def transform(route: Route): Route
  def handle(f: (Unit) ⇒ Route): Route = transform(f(()))
}

object ServePostfix {
  implicit def servePost[x] = new ServePostfix[Post[x], x] {
    def transform(route: Route): Route = post(route)
  }

  implicit def serveGet[x] = new ServePostfix[Get[x], x] {
    def transform(route: Route): Route = get(route)
  }

  implicit def servePut[x] = new ServePostfix[Put[x], x] {
    def transform(route: Route): Route = put(route)
  }

  implicit def serveDelete[x] = new ServePostfix[Delete[x], x] {
    def transform(route: Route): Route = post(route)
  }

  implicit def serveHead[x] = new ServePostfix[Head[x], x] {
    def transform(route: Route): Route = get(route)
  }

  implicit def serveOptions[x] = new ServePostfix[Options[x], x] {
    def transform(route: Route): Route = put(route)
  }

  implicit def servePatch[x] = new ServePostfix[Options[x], x] {
    def transform(route: Route): Route = put(route)
  }
}