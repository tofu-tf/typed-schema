package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.typeDSL._
import shapeless.HList
import shapeless.ops.hlist._

trait ServePrefix[T, I <: HList] extends ServePartial[T, I, Nothing] {
  type Tag
  def handle(f: (I) ⇒ Route): Route
}

trait LowLevelServePrefix {
  implicit def elementServe[T, I <: HList](implicit element: ServeElement[T, I]) = new ServePrefix[T, I] {
    type Tag = Nothing
    def handle(f: (I) ⇒ Route): Route = element.handle(f)
  }
}

object ServePrefix extends LowLevelServePrefix {
  implicit def tagServe[start, tag, I <: HList]
  (implicit start: ServePrefix[start, I]) = new ServePrefix[start, I] {
    type Tag = tag
    def handle(f: (I) ⇒ Route): Route = start.handle(f)
  }

  //TODO consider change order of Prefix and Element in Prepend (they are named anyway)
  //TODO as I2 would be considerably shorter
  implicit def consServe[start, end, I1 <: HList, I2 <: HList]
  (implicit start: ServePrefix[start, I1],
   end: ServeElement[end, I2],
   prepend: Prepend[I2, I1]) =
  new ServePrefix[start :> end, prepend.Out] {
    type Tag = start.Tag
    def handle(f: (prepend.Out) => Route): Route = start.handle(i1 ⇒ end.handle(i2 ⇒ f(prepend(i2, i1))))
  }
}









