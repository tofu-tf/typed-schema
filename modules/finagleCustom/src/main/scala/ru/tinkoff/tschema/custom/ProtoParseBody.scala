package ru.tinkoff.tschema.custom

import cats.Monad
import ru.tinkoff.tschema.finagle.util.message._
import ru.tinkoff.tschema.finagle.{ParseBody, Routed}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

trait ProtoParseBody[F[_], A] extends ParseBody[F, A]

object ProtoParseBody {
  implicit def parseBodyInstance[F[_]: Routed: Monad, A <: GeneratedMessage](implicit
      parser: GeneratedMessageCompanion[A]
  ): ProtoParseBody[F, A] = new ProtoParseBody[F, A] {
    def parse(): F[A]            = parseRequestBytes(req => parser.validate(req).toEither)
    def parseOpt(): F[Option[A]] = parseOptRequestBytes(req => parser.validate(req).toEither)
  }
}
