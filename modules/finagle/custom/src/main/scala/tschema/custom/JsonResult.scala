package tschema.custom
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, Functor}
import tschema.finagle.{Complete, LiftHttp}
import tschema.swagger.MkSwagger
import tschema.typeDSL

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










