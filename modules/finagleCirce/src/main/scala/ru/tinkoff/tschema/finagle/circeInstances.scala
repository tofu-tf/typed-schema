package ru.tinkoff.tschema.finagle
import cats.{Applicative, Functor, Monad}
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Printer}
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.finagle.util.message.{jsonBodyParse, jsonComplete}

object circeInstances {
  private[this] val defaultPrinter = Printer.noSpaces.copy(dropNullValues = true)

  implicit def circeEncodeComplete[F[_]: Applicative, A: Encoder](
      implicit printer: Printer = defaultPrinter
  ): Complete[F, A, A] =
    jsonComplete(_.asJson.printWith(printer))

  implicit def circeEncodeCompleteF[F[_], G[_]: Functor, A: Encoder](
      implicit lift: LiftHttp[F, G],
      printer: Printer = defaultPrinter
  ): Complete[F, A, G[A]] =
    message.fjsonComplete(_.asJson.printWith(printer))

  implicit def circeDecodeParseBody[F[_]: Routed: Monad, A: Decoder]: ParseBody[F, A] =
    jsonBodyParse(decode[A])
}
