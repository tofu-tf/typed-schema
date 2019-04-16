package ru.tinkoff.tschema.finagle
import Routed.implicits._
import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.flatMap._
import com.twitter.finagle.http.{Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Printer}

object circeInstances {
  implicit def circeEncodeComplete[F[_]: Applicative, A: Encoder](implicit printer: Printer): Complete[F, A] =
    a => Response(Version.Http10, Status.Ok, Reader.fromBuf(Buf.Utf8(a.asJson.pretty(printer)))).pure[F]

  implicit def circeDecodeParseBody[F[_]: Routed, A: Decoder]: ParseBody[F, A] =
    () =>
      Routed.request.flatMap(req =>
        decode[A](req.contentString).fold(fail => Routed.reject(Rejection.body(fail.getMessage)), res => res.pure[F]))
}
