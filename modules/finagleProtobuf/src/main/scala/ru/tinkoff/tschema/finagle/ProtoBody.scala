package ru.tinkoff.tschema.finagle

import cats.FlatMap
import ru.tinkoff.tschema.finagle.Serve.{Add, add}
import ru.tinkoff.tschema.swagger.SwaggerMapper
import ru.tinkoff.tschema.typeDSL.{DSLAtom, ReqBody}
import shapeless.HList

class ProtoBody[name, A] extends DSLAtom

object ProtoBody {
  implicit def protoBodyServe[F[_]: FlatMap, name, A, In <: HList](
      implicit A: ProtoParseBody[F, A]
  ): Add[ProtoBody[name, A], F, In, name, A] =
    add[ProtoBody[name, A], F, In, A, name](A.parse())

  implicit def protoBodySwagger[name, A](
      implicit ev: SwaggerMapper[ReqBody[name, A]]
  ): SwaggerMapper[ProtoBody[name, A]] = ev.as[ProtoBody[name, A]]
}
