package ru.tinkoff.tschema.swagger

import java.util.ResourceBundle

import io.circe._

import scala.io.Source

sealed trait SwaggerDescription {
  def string: String
}
case class StaticDescription(string: String) extends SwaggerDescription

case class TraverseDescription(iterable: TraversableOnce[String]) extends SwaggerDescription {
  def string = iterable.mkString
}

case class I18nDescription(key: String)(implicit bundle: ResourceBundle) extends SwaggerDescription {
  def string = bundle.getString(key)
}

case class ResourceDescription(name: String) extends SwaggerDescription {
  def string = Source.fromInputStream(getClass.getResourceAsStream(name)).getLines.mkString
}

object SwaggerDescription {
  implicit val jsonEncoder: Encoder[SwaggerDescription] = Encoder.encodeString.contramap(_.string)
  implicit val jsonDecoder: Decoder[SwaggerDescription] = Decoder.decodeString.map(StaticDescription)
}
