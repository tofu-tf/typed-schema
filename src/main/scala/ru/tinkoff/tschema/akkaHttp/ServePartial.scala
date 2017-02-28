package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.server.Route
import shapeless.{HList, HNil}

trait ServePartial[T, P <: HList] {
  self ⇒
  type Input

  def apply(f: (Input) ⇒ Route, provide: Provide[P]): Route

  def curry(provide: Provide[P]): Provide[Input] = new Provide[Input] {
    def apply(f: (Input) ⇒ Route): Route = self(f, provide)
  }
}

trait Provide[+Input] {
  def apply(f: Input ⇒ Route): Route
}

object Provide{
  val empty: Provide[HNil] = new Provide[HNil] {
    def apply(f: (HNil) ⇒ Route) = f(HNil)
  }
}
