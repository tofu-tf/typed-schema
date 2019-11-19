package ru.tinkoff.tschema.custom
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, ApplicativeError, FlatMap, Functor, Monad, Show}
import io.circe.{Decoder, Encoder, Printer}
import ru.tinkoff.tschema.finagle.Serve.{Add, add}
import ru.tinkoff.tschema.finagle.util.message._
import ru.tinkoff.tschema.finagle.{Complete, LiftHttp, ParseBody, Routed}
import ru.tinkoff.tschema.swagger.{MkSwagger, SwaggerContent, SwaggerMapper, SwaggerPrimitive}
import ru.tinkoff.tschema.{ResponseStatus, typeDSL}
import ru.tinkoff.tschema.typeDSL.{DSLAtom, ReqBody}
import shapeless.HList
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.TokenWriterProducer

/** special complete type for JSON responses, it would try Tethys Writer first, then Circe Encoder
  * you can define your own instance for your types*/
class JsonResult[A]

object JsonResult {
  implicit def jsonComplete[F[_]: Applicative, R, A: AsResponse.Json]: Complete[F, JsonResult[R], A] =
    a => AsResponse.json(a).pure[F]

  implicit def jsonCompleteF[F[_], G[_]: Functor, R, A: AsResponse.Json](
      implicit lift: LiftHttp[F, G]
  ): Complete[F, JsonResult[R], G[A]] =
    fa => lift(fa.map(AsResponse.json[A]))

  implicit def jsonSwagger[R](
      implicit mkSwagger: MkSwagger[typeDSL.Complete[R]]
  ): MkSwagger[typeDSL.Complete[JsonResult[R]]] =
    mkSwagger.as[typeDSL.Complete[JsonResult[R]]]
}

class ExceptResult[E, A]

object ExceptResult {
  implicit def exceptComplete[F[_], R, E, A](
      implicit result: ExceptComplete[F, R, E, A]
  ): Complete[F, ExceptResult[E, R], A] = result

  implicit def exceptSwagger[E, R](
      implicit success: MkSwagger[typeDSL.Complete[R]],
      fail: SwaggerContent[E]
  ): MkSwagger[typeDSL.Complete[ExceptResult[E, R]]] =
    success.addContent(fail).as[typeDSL.Complete[ExceptResult[E, R]]]
}

trait ExceptComplete[F[_], R, E, A] extends Complete[F, ExceptResult[E, R], A]

object ExceptComplete {
  implicit def applicativeErrorComplete[F[_], G[_], R, E, A](
      implicit G: ApplicativeError[G, E],
      F: Monad[F],
      success: Complete[F, R, A],
      error: AsResponse.Error[E],
      lift: LiftHttp[F, G]
  ): ExceptComplete[F, R, E, G[A]] =
    fa => F.flatMap(lift(G.attempt(fa)))(_.fold(error(_).pure[F], success.complete))
}

/** special container for JSON bodies it would try Tethys Reader first, then Circe Decoder
  * you can define your own instance for your types*/
class JsonBody[name, A] extends DSLAtom

object JsonBody {
  implicit def jsonBodyServe[F[_]: FlatMap, name, A, In <: HList](
      implicit A: JsonParseBody[F, A]
  ): Add[JsonBody[name, A], F, In, name, A] =
    add[JsonBody[name, A], F, In, A, name](A.parse())

  implicit def jsonBodySwagger[name, A](
      implicit ev: SwaggerMapper[ReqBody[name, A]]
  ): SwaggerMapper[JsonBody[name, A]] = ev.as[JsonBody[name, A]]
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

class PlainResult[A]

object PlainResult {
  implicit def plainComplete[F[_]: Applicative, A: AsResponse.Plain]: Complete[F, PlainResult[A], A] =
    a => AsResponse.plain(a).pure[F]

  implicit def plainFComplete[F[_], G[_]: Functor, A: AsResponse.Plain](
      implicit lift: LiftHttp[F, G]
  ): Complete[F, PlainResult[A], G[A]] = gs => lift(gs.map(AsResponse.plain[A]))

  implicit def plainSwagger[R]: MkSwagger[typeDSL.Complete[PlainResult[R]]] =
    MkSwagger
      .summon[typeDSL.Complete[String]]
      .addDescribedResponse(200, SwaggerPrimitive.string.withMediaType("text/plain"))
      .as[typeDSL.Complete[PlainResult[R]]]
}
