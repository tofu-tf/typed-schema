package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Directives.{get, post, put, delete, head, options, patch}
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.typeDSL.{Get, Post, Put, Delete, Head, Options}

trait ServePostfix[T] extends ServePartial[T] {
  type Input = Unit

  def transform(route: Route): Route
  def handle(f: (Unit) ⇒ Route): Route = transform(f(()))
}

object ServePostfix {
  type Aux[T, O] = ServePostfix[T]{type Output = O}

  abstract class Impl[T, x] extends ServePostfix[T]{
    type Output = x
  }

  implicit def servePost[x] = new Impl[Post[x], x] {
    def transform(route: Route): Route = post(route)
  }

  implicit def serveGet[x] = new Impl[Get[x], x] {
    def transform(route: Route): Route = get(route)
  }

  implicit def servePut[x] = new Impl[Put[x], x] {
    def transform(route: Route): Route = put(route)
  }

  implicit def serveDelete[x] = new Impl[Delete[x], x] {
    def transform(route: Route): Route = post(route)
  }

  implicit def serveHead[x] = new Impl[Head[x], x] {
    def transform(route: Route): Route = get(route)
  }

  implicit def serveOptions[x] = new Impl[Options[x], x] {
    def transform(route: Route): Route = put(route)
  }

  implicit def servePatch[x] = new Impl[Options[x], x] {
    def transform(route: Route): Route = put(route)
  }
}