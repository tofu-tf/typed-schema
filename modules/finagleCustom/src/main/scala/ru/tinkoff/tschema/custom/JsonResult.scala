package ru.tinkoff.tschema.custom
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, Functor}
import ru.tinkoff.tschema.finagle.{Completing, LiftHttp}
import ru.tinkoff.tschema.swagger.MkSwagger
import ru.tinkoff.tschema.typeDSL

/** special complete type for JSON responses, it would try Tethys Writer first, then Circe Encoder
  * you can define your own instance for your types
  */
class JsonResult[A]

object JsonResult {
  implicit def jsonComplete[F[_]: Applicative, A: AsResponse.Json]: Completing[F, JsonResult[A], A] =
    a => AsResponse.json(a).pure[F]

  implicit def jsonCompleteF[F[_], G[_]: Functor, A: AsResponse.Json](implicit
      lift: LiftHttp[F, G]
  ): Completing[F, JsonResult[A], G[A]] =
    fa => lift(fa.map(AsResponse.json[A]))

  implicit def jsonSwagger[R](implicit
      mkSwagger: MkSwagger[typeDSL.Complete[R]]
  ): MkSwagger[typeDSL.Complete[JsonResult[R]]] =
    mkSwagger.as[typeDSL.Complete[JsonResult[R]]]
}
