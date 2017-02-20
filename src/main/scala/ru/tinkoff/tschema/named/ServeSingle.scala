package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.typeDSL._
import shapeless.HList

trait ServeSingle[T, P <: HList] extends ServePartial[T, P] {
  type Input <: HList
  type Output
  type Key
}

object ServeSingle {
  type Aux[T, P <: HList, I, O] = ServeSingle[T, P] {type Input = I; type Output = O}
  type TAux[T, P <: HList, I, O, Q] = ServeSingle[T, P] {type Input = I; type Output = O; type Key = Q}

  implicit def serveCons[start, end, P <: HList]
  (implicit
   start: ServePrefix[start, P],
   end: ServePostfix[end]) = new ServeSingle[start :> end, P] {
    type Input = start.Input
    type Output = end.Output
    type Key = start.Key
    def apply(f: Input => Route, provide: Provide[P]): Route = start(in ⇒ end(f(in)), provide)
  }
}