package ru.tinkoff.tschema.finagle
import cats.Applicative
import ru.tinkoff.tschema.finagle.util.message.{jsonBodyParse, jsonComplete}
import tethys._
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.TokenWriterProducer

object tethysInstances {
  implicit def tethysEncodeComplete[F[_]: Applicative, A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer): Complete[F, A] =
    jsonComplete(_.asJson)

  implicit def tethysDecodeParseBody[F[_]: Routed, A: JsonReader](
      implicit producer: TokenIteratorProducer = jackson.jacksonTokenIteratorProducer
  ): ParseBody[F, A] =
    jsonBodyParse(_.jsonAs[A])
}
