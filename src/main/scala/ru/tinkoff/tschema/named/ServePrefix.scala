package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, HNil}
import shapeless.ops.hlist._

trait ServePrefix[T] extends ServePartial[T] {
  type Key
  type Input <: HList
  type Output = Nothing
  def handle(f: (Input) ⇒ Route): Route
}

trait LowLevelServePrefix {
  implicit def elementServe[T, I <: HList](implicit element: ServeElement.Aux[T, I]) = new ServePrefix[T] {
    type Key = Nothing
    type Input = I
    def handle(f: (I) ⇒ Route): Route = element.handle(f)
  }
}

object ServePrefix extends LowLevelServePrefix {
  type Aux[T, I] = ServePrefix[T] {type Input = I}
  type KAux[T, I, K] = ServePrefix[T]{type Input = I; type Key = K}

  implicit def keyConsServe[start, key, I <: HList]
  (implicit start: Aux[start, I]) = new ServePrefix[start :> Key[key]] {
    type Input = I
    type Key = key
    def handle(f: (I) ⇒ Route): Route = start.handle(f)
  }

  implicit def keyServe[key] = new ServePrefix[Key[key]] {
    type Input = HNil
    type Key = key
    def handle(f: (Input) ⇒ Route): Route = f(HNil)
  }

  //TODO consider change order of Prefix and Element in Prepend (they are named anyway)
  //TODO as I2 would be considerably shorter
  implicit def consServe[start, end, I1 <: HList, I2 <: HList]
  (implicit start: Aux[start, I1],
   end: ServeElement.Aux[end, I2],
   prepend: Prepend[I2, I1]) =
  new ServePrefix[start :> end] {
    type Input = prepend.Out
    type Key = start.Key
    def handle(f: (prepend.Out) => Route): Route = start.handle(i1 ⇒ end.handle(i2 ⇒ f(prepend(i2, i1))))
  }
}









