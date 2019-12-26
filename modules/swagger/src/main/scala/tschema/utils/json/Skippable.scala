package tschema.utils.json

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import shapeless.tag
import tag.@@
import simulacrum.typeclass

/**
  * tag for types that should be omitted with default value in json
  */
  final abstract class Skippable

object Skippable {
  implicit def skippableEncoder[T: Encoder : Default]: Encoder[T @@ Skippable] = Encoder.instance {
    case x if Default[T].value == x => Json.Null
    case x                          => Encoder[T].apply(x)
  }
  implicit def skippableDecoder[T: Decoder : Default]: Decoder[T @@ Skippable] = Decoder.instance {
    h => if (h.value.isNull) Right(tag[Skippable](Default[T].value)) else h.as[T].asInstanceOf[Decoder.Result[T @@ Skippable]]
  }

  implicit def tagImplicitly[T](x: T): T @@ Skippable = tag[Skippable](x)
}

@typeclass trait Default[T] {
  def value: T
}

object Default {
  implicit val defaultBoolean: Default[Boolean] = new Default[Boolean] {
    def value: Boolean = false
  }
  implicit def defaultList[A]: Default[List[A]] = new Default[List[A]] {
    def value: List[A] = List.empty
  }
}


