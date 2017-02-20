package ru.tinkoff.tschema.named

import shapeless.labelled.FieldType
import shapeless.{:+:, CNil, Coproduct}

trait Routable1[In, Out] {
  def route(in: In): Unit
}

object Routable1 {
  def apply[In, Out](implicit routable: Routable1[In, Out]) = routable

  implicit val cnilRoutable: Routable1[CNil, CNil] = new Routable1[CNil, CNil] {
    def route(res: CNil) = res.impossible
  }

  implicit def coproductRoutable[K, I, O, IT <: Coproduct, OT <: Coproduct]
  (implicit head: Routable1[I, O], tail: Routable1[IT, OT]): Routable1[FieldType[K, I] :+: IT, FieldType[K, O] :+: OT] =
    new Routable1[FieldType[K, I] :+: IT, FieldType[K, O] :+: OT] {
      def route(res: FieldType[K, I] :+: IT) = ()
    }

  implicit def single[A]: Routable1[A, A] =
    new Routable1[A, A] {
      def route(res: A) = ()
    }

  implicit def option[A]: Routable1[Option[A], A] =
    new Routable1[Option[A], A] {
      def route(res: Option[A]) = ()
    }
}



