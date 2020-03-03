package ru.tinkoff.tschema.finagle
import cats.{Applicative, Functor, Monad}
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.finagle.util.message.{jsonBodyParse, jsonComplete}
import tethys._
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.TokenWriterProducer

object tethysInstances {
  implicit def tethysEncodeComplete[F[_]: Applicative, A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer): Completing[F, A, A] =
    jsonComplete(_.asJson)

  implicit def tethysEncodeCompleteF[F[_], G[_]: Functor, A: JsonWriter](
      implicit lift: LiftHttp[F, G],
      producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer): Completing[F, A, G[A]] =
    message.fjsonComplete(_.asJson)

  implicit def tethysDecodeParseBody[F[_]: Routed: Monad, A: JsonReader](
      implicit producer: TokenIteratorProducer = jackson.jacksonTokenIteratorProducer
  ): ParseBody[F, A] =
    jsonBodyParse(_.jsonAs[A])
}
