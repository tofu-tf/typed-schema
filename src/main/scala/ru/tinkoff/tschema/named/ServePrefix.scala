package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, HNil}
import shapeless.ops.hlist._

trait ServePrefix[T, P <: HList] extends ServePartial[T, P] {
  type Key
  type Input <: HList
}

trait LowLevelServePrefix {
  implicit def elementServe[T, P <: HList, I <: HList]
  (implicit element: ServeElement.Aux[T, I],
   prepend: Prepend[I, P]) = new ServePrefix[T, P] {
    type Key = Nothing
    type Input = prepend.Out
    def apply(f: Input ⇒ Route, provide: Provide[P]): Route = provide(p ⇒ element(in ⇒ f(prepend(in, p))))
  }
}

object ServePrefix extends LowLevelServePrefix {
  type Aux[T, P <: HList, I] = ServePrefix[T, P] {type Input = I}
  type KAux[T, P <: HList, I, K] = ServePrefix[T, P] {type Input = I; type Key = K}

  implicit def keyConsServe[start, key, P <: HList, I <: HList]
  (implicit start: Aux[start, P, I]) = new ServePrefix[start :> Key[key], P] {
    type Input = I
    type Key = key
    def apply(f: (I) ⇒ Route, p: Provide[P]): Route = start(f, p)
  }

  implicit def keyServe[key, P <: HList] = new ServePrefix[Key[key], P] {
    type Input = P
    type Key = key
    def apply(f: (Input) ⇒ Route, p: Provide[P]): Route = p(in ⇒ f(in))
  }

  //TODO consider change order of Prefix and Element in Prepend (they are named anyway)
  //TODO as I2 would be considerably shorter
  implicit def elementConsServe[start, elem, P <: HList, I1 <: HList, I2 <: HList]
  (implicit start: Aux[start, P, I1],
   end: ServeElement.Aux[elem, I2],
   prepend: Prepend[I2, I1]) =
  new ServePrefix[start :> elem, P] {
    type Input = prepend.Out
    type Key = start.Key
    def apply(f: (prepend.Out) => Route, provide: Provide[P]): Route = start(i1 ⇒ end(i2 ⇒ f(prepend(i2, i1))), provide)
  }

  implicit def middleConsServe[start, mid, P <: HList, I <: HList]
  (implicit start: Aux[start, P, I], mid: ServeMiddle[mid, I]): Aux[start :> mid, P, I] =
    new ServePrefix[start :> mid, P] {
      type Key = start.Key
      type Input = start.Input
      def apply(f: Input ⇒ Route, provide: Provide[P]): Route =
        start(in ⇒ mid(f, start.curry(provide)), provide)
    }
}









