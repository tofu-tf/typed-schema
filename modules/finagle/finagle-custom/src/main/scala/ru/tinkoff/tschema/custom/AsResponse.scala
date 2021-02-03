package ru.tinkoff.tschema.custom
import cats.Show
import com.twitter.finagle.http.{Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import derevo.Derivation
import io.circe.{Encoder, Printer}
import magnolia.{CaseClass, Magnolia, SealedTrait}
import ru.tinkoff.tschema.ResponseStatus
import ru.tinkoff.tschema.finagle.util.message.{jsonResponse, stringResponse}
import tethys.writers.tokens.TokenWriterProducer

import scala.annotation.implicitNotFound

sealed trait EntityFormat

class BinaryContentType(val name: String)

object EntityFormat {
  final abstract class Json       extends EntityFormat
  final abstract class Plain      extends EntityFormat
  final abstract class Binary[CT] extends EntityFormat
  final abstract class XML        extends EntityFormat
  final abstract class Error      extends EntityFormat
}

trait AsResponse[Fmt, A] {
  def apply(a: A): Response
}

object AsResponse extends AsResponseInstances with AsResponseDerivation {
  type Json[A]       = AsResponse[EntityFormat.Json, A]
  type Plain[A]      = AsResponse[EntityFormat.Plain, A]
  type Binary[CT, A] = AsResponse[EntityFormat.Binary[CT], A]
  type XML[A]        = AsResponse[EntityFormat.XML, A]
  type Error[A]      = AsResponse[EntityFormat.Error, A]

  def json[A](a: A)(implicit asResponse: Json[A]): Response             = asResponse(a)
  def plain[A](a: A)(implicit asResponse: Plain[A]): Response           = asResponse(a)
  def binary[CT, A](a: A)(implicit asResponse: Binary[CT, A]): Response = asResponse(a)
  def xml[A](a: A)(implicit asResponse: XML[A]): Response               = asResponse(a)
  def error[A](a: A)(implicit asResponse: Error[A]): Response           = asResponse(a)

  import tethys._
  implicit def tethysAsResponse[A: JsonWriter](implicit
      producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer
  ): AsResponse.Json[A] = a => jsonResponse(a.asJson)

  implicit val plainAsResponseString: AsResponse.Plain[String] = stringResponse(_)
  implicit val plainAsResponseUnit: AsResponse.Plain[Unit]     = _ => Response(Status.Ok)

  implicit def binAsResponseByteSeq[T[x] <: Seq[x], CT <: BinaryContentType](implicit
      ct: CT
  ): AsResponse.Binary[CT, T[Byte]] =
    bytes => {
      val resp = Response(Status.Ok)
      resp.contentType = ct.name
      resp.content(Buf.ByteArray(bytes: _*))
      resp
    }

  implicit def binAsResponseBuf[CT <: BinaryContentType](implicit
      ct: CT
  ): AsResponse.Binary[CT, Buf] =
    buf => {
      val resp = Response(Status.Ok)
      resp.contentType = ct.name
      resp.content(buf)
      resp
    }

  implicit def binAsResponseReader[CT <: BinaryContentType](implicit
      ct: CT
  ): AsResponse.Binary[CT, Reader[Buf]] =
    reader => {
      val resp = Response(Version.Http11, Status.Ok, reader)
      resp.contentType = ct.name
      resp
    }
}

trait AsResponseInstances {
  import cats.syntax.show._
  import io.circe.syntax._
  private[this] val defaultPrinter = Printer.noSpaces.copy(dropNullValues = true)

  implicit def circeAsResponse[A: Encoder](implicit
      printer: Printer = defaultPrinter
  ): AsResponse.Json[A] = a => jsonResponse(a.asJson.printWith(printer))

  implicit def showAsResponse[A: Show]: AsResponse.Plain[A] = a => stringResponse(a.show)
}

trait AsResponseDerivation {
  object Error extends AsResponseMagnolia[EntityFormat.Error] with Derivation[AsResponse.Error]
  object Plain extends AsResponseMagnolia[EntityFormat.Plain] with Derivation[AsResponse.Plain]
  object XML   extends AsResponseMagnolia[EntityFormat.XML] with Derivation[AsResponse.XML]
  object Json  extends AsResponseMagnolia[EntityFormat.Json] with Derivation[AsResponse.Json]
}

abstract class ErrorResponse[T](val status: Int) extends ResponseStatus[T] with AsResponse.Error[T]

trait AsResponseMagnolia[Fmt] {
  type Typeclass[A] = AsResponse[Fmt, A]

  def combine[T](cc: CaseClass[Typeclass, T])(implicit no: AsResponseNotCombineable) = no.absurd

  def dispatch[T](st: SealedTrait[Typeclass, T]): AsResponse[Fmt, T] =
    a => st.dispatch(a)(sub => sub.typeclass.apply(sub.cast(a)))

  def instance[A]: AsResponse[Fmt, A] = macro Magnolia.gen[A]
}

@implicitNotFound("AsResponse could be derived only for sealed traits")
final abstract class AsResponseNotCombineable {
  def absurd: Nothing
}
