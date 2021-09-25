package ru.tinkoff.tschema.utils.json

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import shapeless.tag
import tag.@@

/** tag for types that should be omitted with default value in json
  */
final abstract class Skippable

object Skippable {
  implicit def skippableEncoder[T: Encoder: Default]: Encoder[T @@ Skippable] = Encoder.instance {
    case x if Default[T].value == x => Json.Null
    case x                          => Encoder[T].apply(x)
  }
  implicit def skippableDecoder[T: Decoder: Default]: Decoder[T @@ Skippable] = Decoder.instance { h =>
    if (h.value.isNull) Right(tag[Skippable](Default[T].value))
    else h.as[T].asInstanceOf[Decoder.Result[T @@ Skippable]]
  }

  implicit def tagImplicitly[T](x: T): T @@ Skippable                         = tag[Skippable](x)
}

trait Default[T] {
  def value: T
}

object Default {
  def apply[A](implicit instance: Default[A]): Default[A] = instance

  trait Ops[A] {
    def typeClassInstance: Default[A]
    def self: A
  }

  trait ToDefaultOps {
    implicit def toDefaultOps[A](target: A)(implicit tc: Default[A]): Ops[A] = new Ops[A] {
      val self              = target
      val typeClassInstance = tc
    }
  }

  trait AllOps[A] extends Ops[A] {
    def typeClassInstance: Default[A]
  }

  object ops {
    implicit def toAllDefaultOps[A](target: A)(implicit tc: Default[A]): AllOps[A] = new AllOps[A] {
      val self              = target
      val typeClassInstance = tc
    }
  }

  implicit val defaultBoolean: Default[Boolean] = new Default[Boolean] {
    def value: Boolean = false
  }
  implicit def defaultList[A]: Default[List[A]] = new Default[List[A]] {
    def value: List[A] = List.empty
  }
}
