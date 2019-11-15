package ru.tinkoff.tschema.custom
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, FlatMap, Functor, Monad, MonadError}
import io.circe.{Decoder, Encoder, Printer}
import ru.tinkoff.tschema.finagle.Serve.{Add, add}
import ru.tinkoff.tschema.finagle.util.message._
import ru.tinkoff.tschema.finagle.{Complete, LiftHttp, ParseBody, Routed}
import ru.tinkoff.tschema.param.Param.Result
import ru.tinkoff.tschema.swagger.{MkSwagger, SwaggerContent}
import ru.tinkoff.tschema.typeDSL.ReqBody
import shapeless.HList
import tethys.readers.tokens.TokenIteratorProducer
import tethys.{JsonReader, JsonWriter, jackson}
import tethys.writers.tokens.TokenWriterProducer

/** special complete type for JSON responses, it would try Tethys Writer first, then Circe Encoder
  * you can define your own instance for your types*/
class JsonResult[A]

object JsonResult {
  implicit def jsonComplete[F[_], R, A](implicit result: JsonComplete[F, R, A]): Complete[F, JsonResult[R], A] = result
  implicit def jsonSwagger[R](implicit mkSwagger: MkSwagger[Result[R]]): MkSwagger[JsonResult[R]] =
    mkSwagger.as[JsonResult[R]]
}

class ErrorResult[E, A]

object ErrorResult {
  implicit def errorComplete[F[_], R, E, A](
      implicit result: ErrorComplete[F, R, E, A]
  ): Complete[F, ErrorResult[E, R], A] = result

  implicit def errorSwagger[E, R](
      implicit success: MkSwagger[Result[R]],
      fail: SwaggerContent[E]
  ): MkSwagger[ErrorResult[E, R]] =
    success.addContent(fail).as[ErrorResult[E, R]]
}

/** typeclass checking that A could be served as JSON when declared as R
  *  it would try Tethys Writer first, then Circe Encoder*/
trait JsonComplete[F[_], R, A] extends Complete[F, JsonResult[R], A] {}

object JsonComplete extends JsonComplete1 {
  import tethys._
  implicit def tethysEncodeComplete[F[_]: Applicative, A: JsonWriter](
      implicit producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer
  ): JsonComplete[F, A, A] = a => jsonResponse(a.asJson).pure[F]

  implicit def tethysEncodeCompleteF[F[_], G[_]: Functor, A: JsonWriter](
      implicit lift: LiftHttp[F, G],
      producer: TokenWriterProducer = jackson.jacksonTokenWriterProducer
  ): JsonComplete[F, A, G[A]] = fa => lift(fa.map(a => jsonResponse(a.asJson)))
}

trait JsonComplete1 {
  import io.circe.syntax._
  private[this] val defaultPrinter = Printer.noSpaces.copy(dropNullValues = true)

  implicit def circeEncodeComplete[F[_]: Applicative, A: Encoder](
      implicit printer: Printer = defaultPrinter
  ): JsonComplete[F, A, A] =
    a => jsonResponse(a.asJson.printWith(printer)).pure[F]

  implicit def circeEncodeCompleteF[F[_], G[_]: Functor, A: Encoder](
      implicit lift: LiftHttp[F, G],
      printer: Printer = defaultPrinter
  ): JsonComplete[F, A, G[A]] =
    fa => lift(fa.map(a => jsonResponse(a.asJson.printWith(printer))))
}

trait ErrorComplete[F[_], R, E, A] extends Complete[F, ErrorResult[E, R], A]

object ErrorComplete {
  implicit def monadErrorComplete[F[_], R, E, A](
      implicit F: MonadError[F, E],
      success: Complete[F, R, A],
      error: Complete[F, R, E]
  ): ErrorComplete[F, R, E, F[A]] =
    fa => F.handleErrorWith(F.flatMap(fa)(success.complete))(error.complete)
}

/** special container for JSON bodies it would try Tethys Reader first, then Circe Decoder
  * you can define your own instance for your types*/
class JsonBody[name, A]

object JsonBody {
  implicit def jsonBodyServe[F[_]: FlatMap, name, A, In <: HList](
      implicit A: JsonParseBody[F, A]
  ): Add[ReqBody[name, A], F, In, name, A] =
    add[ReqBody[name, A], F, In, A, name](A.parse())
}

/** typeclass checking that A could be parsed as JSON
  *  it would try Tethys Reader first, then Circe Decoder*/
trait JsonParseBody[F[_], A] extends ParseBody[F, A]

object JsonParseBody extends JsonParseBody1 {
  import tethys._
  implicit def tethysDecodeParseBody[F[_]: Routed: Monad, A: JsonReader](
      implicit producer: TokenIteratorProducer = jackson.jacksonTokenIteratorProducer
  ): JsonParseBody[F, A] =
    () => parseRequest(_.jsonAs[A])
}

trait JsonParseBody1 {
  import io.circe.parser._
  implicit def circeDecodeParseBody[F[_]: Routed: Monad, A: Decoder]: JsonParseBody[F, A] =
    () => parseRequest(decode[A])
}
