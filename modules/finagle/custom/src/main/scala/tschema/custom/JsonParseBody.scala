package tschema.custom
import cats.Monad
import io.circe.Decoder
import tschema.finagle.util.message._
import tschema.finagle.{ParseBody, Routed}
import tethys.readers.tokens.TokenIteratorProducer

/** typeclass checking that A could be parsed as JSON
  *  it would try Tethys Reader first, then Circe Decoder*/
trait JsonParseBody[F[_], A] extends ParseBody[F, A]

object JsonParseBody extends JsonParseBody1 {
  import tethys._
  implicit def tethysDecodeParseBody[F[_]: Routed: Monad, A: JsonReader](
      implicit producer: TokenIteratorProducer = jackson.jacksonTokenIteratorProducer
  ): JsonParseBody[F, A] =
    new JsonParseBody[F, A] {
      override def parse(): F[A] = parseRequest(_.jsonAs[A])
      override def parseOpt(): F[Option[A]] = parseOptRequest(_.jsonAs[A])
    }
}

trait JsonParseBody1 {
  import io.circe.parser._
  implicit def circeDecodeParseBody[F[_]: Routed: Monad, A: Decoder]: JsonParseBody[F, A] =
    new JsonParseBody[F, A] {
      override def parse(): F[A] = parseRequest(decode[A])
      override def parseOpt(): F[Option[A]] = parseOptRequest(decode[A])
    }
}
