package ru.tinkoff.tschema.named

import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.macros.NamedImpl
import shapeless.{:+:, CNil, Coproduct, HNil, Inl, Inr}

trait Routable[In , Impl] {
  type Out
  def routeWith(in: In, impl: Impl): Route
}

object Routable {
  type Aux[I, T, O] = Routable[I, T]{type Out = O}

  implicit def routableNamed[In, Impl, Res]
  (implicit named: NamedImpl.Aux[Impl, In, Res],
   result: RoutableResult[Res]): Aux[In, Impl, named.Output] = new Routable[In, Impl] {
    type Out = named.Output
    def routeWith(in: In, impl: Impl) = result.route(named.produce(in, impl))
  }
}

trait RoutableResult[T] {
  def route(res: T): Route
}

object RoutableResult {
  implicit def singleResult[A]
  (implicit marshaller: ToResponseMarshaller[A]) =
    new RoutableResult[A] {
      def route(res: A): Route = complete(res)
    }

  implicit def singleFutureServable[A]
  (implicit marshaller: ToResponseMarshaller[A]) =
    new RoutableResult[A] {
      def route(res: A): Route = complete(res)
    }

  implicit val cnilResult = new RoutableResult[CNil] {
    def route(res: CNil): Route = res.impossible
  }

  implicit def coproductResult[H, T <: Coproduct]
    (implicit head: RoutableResult[H], tail: RoutableResult[T]) = new RoutableResult[H :+: T] {
    def route(res: H :+: T) = res match {
      case Inl(h) ⇒ head.route(h)
      case Inr(t) ⇒ tail.route(t)
    }
  }
}


