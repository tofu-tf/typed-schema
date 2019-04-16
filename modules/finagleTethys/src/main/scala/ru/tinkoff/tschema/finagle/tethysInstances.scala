package ru.tinkoff.tschema.finagle
import Routed.implicits._
import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.flatMap._
import com.twitter.finagle.http.{Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import tethys._
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.TokenWriterProducer

object tethysInstances {
  implicit def tethysEncodeComplete[F[_]: Applicative, A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer): Complete[F, A] =
    a => Response(Version.Http10, Status.Ok, Reader.fromBuf(Buf.Utf8(a.asJson))).pure[F]

  implicit def tethysDecodeParseBody[F[_]: Routed, A: JsonReader](
      implicit producer: TokenIteratorProducer = jackson.jacksonTokenIteratorProducer
  ): ParseBody[F, A] =
    () =>
      Routed.request.flatMap(req =>
        req.contentString.jsonAs[A].fold(fail => Routed.reject(Rejection.body(fail.getMessage)), res => res.pure[F]))
}
