package ru.tinkoff.tschema.custom
import cats.Show
import com.twitter.finagle.http.Response
import io.circe.{Encoder, Printer}
import ru.tinkoff.tschema.ResponseStatus
import ru.tinkoff.tschema.finagle.util.message.{jsonResponse, stringResponse}
import tethys.writers.tokens.TokenWriterProducer

sealed trait EntityFormat

object EntityFormat {
  final abstract class Json   extends EntityFormat
  final abstract class Plain  extends EntityFormat
  final abstract class Binary extends EntityFormat
  final abstract class XML    extends EntityFormat
  final abstract class Error  extends EntityFormat
}

trait AsResponse[Fmt, A] {
  def apply(a: A): Response
}

object AsResponse extends AsResponseInstances {
  type Json[A]   = AsResponse[EntityFormat.Json, A]
  type Plain[A]  = AsResponse[EntityFormat.Plain, A]
  type Binary[A] = AsResponse[EntityFormat.Binary, A]
  type XML[A]    = AsResponse[EntityFormat.XML, A]
  type Error[A]  = AsResponse[EntityFormat.Error, A]

  def json[A](a: A)(implicit asResponse: Json[A]): Response     = asResponse(a)
  def plain[A](a: A)(implicit asResponse: Plain[A]): Response   = asResponse(a)
  def binary[A](a: A)(implicit asResponse: Binary[A]): Response = asResponse(a)
  def xml[A](a: A)(implicit asResponse: XML[A]): Response       = asResponse(a)
  def error[A](a: A)(implicit asResponse: Error[A]): Response   = asResponse(a)

  import tethys._
  implicit def tethysAsResponse[A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer
  ): AsResponse.Json[A] = a => jsonResponse(a.asJson)

  implicit val plainAsResponse: AsResponse.Plain[String] = stringResponse(_)
}

trait AsResponseInstances {
  import cats.syntax.show._
  import io.circe.syntax._
  private[this] val defaultPrinter = Printer.noSpaces.copy(dropNullValues = true)

  implicit def circeAsResponse[A: Encoder](
      implicit printer: Printer = defaultPrinter
  ): AsResponse.Json[A] = a => jsonResponse(a.asJson.printWith(printer))

  implicit def showAsResponse[A: Show]: AsResponse.Plain[A] = a => stringResponse(a.show)
}

abstract class ErrorResponse[T](val status: Int) extends ResponseStatus[T] with AsResponse.Error[T]
