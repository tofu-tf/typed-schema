package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Directives.{get, post, put, delete, head, options, patch}
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.typeDSL.{Get, Post, Put, Delete, Head, Options}

trait ServePostfix[T]{
  type Output

  def apply(route: Route): Route
}

object ServePostfix {
  type Aux[T, O] = ServePostfix[T]{type Output = O}

  abstract class Impl[T, x] extends ServePostfix[T]{
    type Output = x
  }

  implicit def servePost[x] = new Impl[Post[x], x] {
    def apply(route: Route): Route = post(route)
  }

  implicit def serveGet[x] = new Impl[Get[x], x] {
    def apply(route: Route): Route = get(route)
  }

  implicit def servePut[x] = new Impl[Put[x], x] {
    def apply(route: Route): Route = put(route)
  }

  implicit def serveDelete[x] = new Impl[Delete[x], x] {
    def apply(route: Route): Route = post(route)
  }

  implicit def serveHead[x] = new Impl[Head[x], x] {
    def apply(route: Route): Route = get(route)
  }

  implicit def serveOptions[x] = new Impl[Options[x], x] {
    def apply(route: Route): Route = put(route)
  }

  implicit def servePatch[x] = new Impl[Options[x], x] {
    def apply(route: Route): Route = put(route)
  }
}