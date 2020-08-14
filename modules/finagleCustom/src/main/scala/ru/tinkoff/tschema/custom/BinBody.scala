package ru.tinkoff.tschema.custom

import cats.FlatMap
import ru.tinkoff.tschema.finagle.Serve.{Add, add}
import ru.tinkoff.tschema.swagger.SwaggerMapper
import ru.tinkoff.tschema.typeDSL.{DSLAtom, ReqBody}
import shapeless.HList

class BinBody[name, A] extends DSLAtom

object BinBody {
  implicit def binBodyServe[F[_]: FlatMap, name, In <: HList](implicit
      A: BinParseBody[F, Array[Byte]]
  ): Add[BinBody[name, Array[Byte]], F, In, name, Array[Byte]] =
    add[BinBody[name, Array[Byte]], F, In, Array[Byte], name](A.parse())

  implicit def binBodySwagger[name](implicit
      ev: SwaggerMapper[ReqBody[name, Array[Byte]]]
  ): SwaggerMapper[BinBody[name, Array[Byte]]] = ev.as[BinBody[name, Array[Byte]]]
}
