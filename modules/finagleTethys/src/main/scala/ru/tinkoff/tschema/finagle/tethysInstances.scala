package ru.tinkoff.tschema.finagle
import cats.{Applicative, Monad}
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.finagle.util.message.{jsonBodyParse, jsonComplete}
import tethys._
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.TokenWriterProducer

object tethysInstances {
  implicit def tethysEncodeComplete[F[_]: Applicative, A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer): Complete[F, A] =
    jsonComplete(_.asJson)

  implicit def tethysEncodeCompleteF[F[_]: Applicative, A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer): Complete[F, F[A]] =
    message.fjsonComplete(_.asJson)

  implicit def tethysDecodeParseBody[F[_]: Routed: Monad, A: JsonReader](
      implicit producer: TokenIteratorProducer = jackson.jacksonTokenIteratorProducer
  ): ParseBody[F, A] =
    jsonBodyParse(_.jsonAs[A])
}
