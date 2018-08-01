package ru.tinkoff


import ru.tinkoff.tschema.swagger.GenericSwaggerTypeable.HListProps
import ru.tinkoff.tschema.swagger.{SwaggerPrimitive, SwaggerTypeable}
import shapeless.ops.hlist.Take
import shapeless.{LabelledGeneric, Nat, the}
import shapeless.tag._
import io.circe.syntax._
import shapeless.record.Record

object SomethingsDebug extends App{
  type Id[X] = String @@ X
  implicit def idTypeable[A]: SwaggerTypeable[Id[A]] = SwaggerTypeable.make(SwaggerPrimitive.string).as[Id[A]]
  the[HListProps[Record.`'name -> String, 'id -> Id[Group]`.T]]
  case class Group(id: Id[Group],
                   name: String,
                   icon: String,
                   rating: BigInt)
  implicit def groupTypeable: SwaggerTypeable[Group] = SwaggerTypeable.genTypeable[Group]

  println(groupTypeable.typ.asJson)
}
