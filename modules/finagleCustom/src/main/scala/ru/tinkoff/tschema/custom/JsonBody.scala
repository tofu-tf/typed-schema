package ru.tinkoff.tschema.custom
import cats.FlatMap
import ru.tinkoff.tschema.finagle.Serve.{Add, add}
import ru.tinkoff.tschema.swagger.SwaggerMapper
import ru.tinkoff.tschema.typeDSL.{DSLAtom, ReqBody}
import shapeless.HList

/** special container for JSON bodies it would try Tethys Reader first, then Circe Decoder you can define your own
  * instance for your types
  */
class JsonBody[name, A] extends DSLAtom

object JsonBody {
  implicit def jsonBodyServe[F[_]: FlatMap, name, A, In <: HList](implicit
      A: JsonParseBody[F, A]
  ): Add[JsonBody[name, A], F, In, name, A] =
    add[JsonBody[name, A], F, In, A, name](A.parse())

  implicit def jsonBodySwagger[name, A](implicit
      ev: SwaggerMapper[ReqBody[name, A]]
  ): SwaggerMapper[JsonBody[name, A]] = ev.as[JsonBody[name, A]]
}
