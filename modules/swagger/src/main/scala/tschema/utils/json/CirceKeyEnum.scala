package tschema.utils.json

import enumeratum.{Enum, EnumEntry}
import io.circe._

trait CirceKeyEnum[T <: EnumEntry] {
  self: Enum[T] =>
  implicit val circeKeyEncoder: KeyEncoder[T] = KeyEncoder.encodeKeyString.contramap(_.entryName)
  implicit val circeKeyDecoder: KeyDecoder[T] = KeyDecoder.decodeKeyString.map(withName)
}
