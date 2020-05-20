package ru.tinkoff.tschema.utils.json

import akka.http.scaladsl.marshalling.{Marshaller, _}
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.{HttpEntity, HttpRequest}
import akka.http.scaladsl.unmarshalling.{Unmarshaller, _}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import cats.syntax.either._

import scala.concurrent.Future

object CirceSupport {
  val printer = Printer.noSpaces.copy(dropNullValues = true)

  def marshallResponse[T: Encoder]: ToResponseMarshaller[T] = Marshaller.fromToEntityMarshaller[T]()(marshallEntity)

  def unmarshallRequest[T: Decoder]: FromRequestUnmarshaller[T] =
    Unmarshaller
      .identityUnmarshaller[HttpRequest]
      .map(_.entity)
      .flatMap[T](unmarshallEntity[T]: Unmarshaller[HttpEntity, T])
      .asScala

  def marshallEntity[T: Encoder]: ToEntityMarshaller[T] =
    Marshaller.stringMarshaller(`application/json`).compose((_: T).asJson.pretty(printer))

  def unmarshallEntity[T: Decoder]: FromEntityUnmarshaller[T] =
    Unmarshaller.stringUnmarshaller
      .forContentTypes(`application/json`)
      .flatMap(implicit ec => _ => s => Future.fromTry(parse(s).toTry.flatMap(_.as[T].toTry)))

  object implicits {
    implicit def marshallResponseCirce[T: Encoder]: ToResponseMarshaller[T] = marshallResponse[T]

    implicit def unmarshallRequestCirce[T: Decoder]: FromRequestUnmarshaller[T] = unmarshallRequest[T]

    implicit def marshallEntityCirce[T: Encoder]: ToEntityMarshaller[T] = marshallEntity[T]

    implicit def unmarshallEntityCirce[T: Decoder]: FromEntityUnmarshaller[T] = unmarshallEntity[T]
  }

}
