package ru.tinkoff.tschema.custom

import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, Functor}
import ru.tinkoff.tschema.finagle.{Completing, LiftHttp}
import ru.tinkoff.tschema.swagger.SwaggerPrimitive
import ru.tinkoff.tschema.typeDSL
import tschema.swagger.MkSwagger

class BinResult[CT, A]

object BinResult {
  implicit def binComplete[CT, F[_]: Applicative, A: AsResponse.Binary[CT, *]]: Completing[F, BinResult[CT, A], A] =
    a => AsResponse.binary(a).pure[F]

  implicit def binFComplete[CT, F[_], G[_]: Functor, A: AsResponse.Binary[CT, *]](
      implicit lift: LiftHttp[F, G]
  ): Completing[F, BinResult[CT, A], G[A]] = gs => lift(gs.map(AsResponse.binary[CT, A]))

  implicit def binSwagger[TC <: BinaryContentType, R](implicit tc: TC): MkSwagger[typeDSL.Complete[BinResult[TC, R]]] =
    MkSwagger
      .summon[typeDSL.Complete[String]]
      .addDescribedResponse(200, SwaggerPrimitive.string.withMediaType(tc.name))
      .as[typeDSL.Complete[BinResult[TC, R]]]
}
