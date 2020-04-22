package ru.tinkoff.tschema.custom

import cats.Monad
import ru.tinkoff.tschema.finagle.util.message.{parseOptRequestBytes, parseRequestBytes}
import ru.tinkoff.tschema.finagle.{ParseBody, Routed}
import cats.syntax.either._

trait BinParseBody[F[_], A] extends ParseBody[F, Array[Byte]]

object BinParseBody {
  implicit def parseBodyInstance[F[_]: Routed: Monad, A]: BinParseBody[F, A] = new BinParseBody[F, A] {
    def parse(): F[Array[Byte]]            = parseRequestBytes(_.asRight[Throwable])
    def parseOpt(): F[Option[Array[Byte]]] = parseOptRequestBytes(_.asRight[Throwable])
  }
}
