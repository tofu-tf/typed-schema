package ru.tinkoff.tschema.named

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import shapeless.labelled.FieldType
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

import scala.concurrent.Future

trait RoutableUnion[In <: Coproduct, Out <: Coproduct] {
  def route(in: In): Route
}

object RoutableUnion {
  def apply[In <: Coproduct, Out <: Coproduct](implicit routable: RoutableUnion[In, Out]) = routable

  implicit val cnilRoutable: RoutableUnion[CNil, CNil] = new RoutableUnion[CNil, CNil] {
    def route(res: CNil): Route = res.impossible
  }

  implicit def coproductRoutable[K, I, O, IT <: Coproduct, OT <: Coproduct]
  (implicit head: Routable[I, O], tail: RoutableUnion[IT, OT]): RoutableUnion[FieldType[K, I] :+: IT, FieldType[K, O] :+: OT] =
    new RoutableUnion[FieldType[K, I] :+: IT, FieldType[K, O] :+: OT] {
      def route(res: FieldType[K, I] :+: IT) = res match {
        case Inl(h) ⇒ head.route(h: I)
        case Inr(t) ⇒ tail.route(t)
      }
    }
}

trait Routable[In, Out]{
  def route(in: In): Route
}

object Routable{
  implicit def single[A](implicit marshaller: ToResponseMarshaller[A]): Routable[A, A] =
    new Routable[A, A] {
      def route(res: A): Route = complete(res)
    }

  implicit def future[A](implicit marshaller: ToResponseMarshaller[A]): Routable[Future[A], A] =
    new Routable[Future[A], A] {
      def route(res: Future[A]): Route = complete(res)
    }
}


