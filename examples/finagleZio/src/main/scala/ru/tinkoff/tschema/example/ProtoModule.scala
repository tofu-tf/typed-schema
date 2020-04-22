package ru.tinkoff.tschema.example

import magebook.MageBook
import ru.tinkoff.tschema.finagle.protoInstances._
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.{MkSwagger, SwaggerPrimitive, SwaggerType, SwaggerTypeable}
import ru.tinkoff.tschema.finagle.circeInstances._
import ru.tinkoff.tschema.syntax._

object ProtoModule extends ExampleModule {

  implicit val mageBookTypeable: SwaggerTypeable[MageBook] = new SwaggerTypeable[MageBook] {
    override def typ: SwaggerType = SwaggerPrimitive.binary
  }

  def api =
    post |>
      operation("proto") |>
      protoBody[MageBook]("book") |>
      $$[Int]

  object handler {
    def proto(book: MageBook): Int = book.enchantment * book.potion * book.spell
  }

  val swag = MkSwagger(api)
  val route = MkService[Http](api)(handler)
}
