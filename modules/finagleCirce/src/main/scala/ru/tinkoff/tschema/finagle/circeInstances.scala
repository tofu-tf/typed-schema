package ru.tinkoff.tschema.finagle
import cats.Applicative
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Printer}
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.finagle.util.message.{jsonBodyParse, jsonComplete}

object circeInstances {
  implicit def circeEncodeComplete[F[_]: Applicative, A: Encoder](implicit printer: Printer): Complete[F, A] =
    jsonComplete(_.asJson.pretty(printer))

  implicit def circeEncodeCompleteF[F[_]: Applicative, A: Encoder](implicit printer: Printer): Complete[F, F[A]] =
    message.fjsonComplete(_.asJson.pretty(printer))

  implicit def circeDecodeParseBody[F[_]: Routed, A: Decoder]: ParseBody[F, A] =
    jsonBodyParse(decode[A])
}
