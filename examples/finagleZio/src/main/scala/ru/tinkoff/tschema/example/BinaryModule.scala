package ru.tinkoff.tschema.example

import java.nio.charset.StandardCharsets
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.MkSwagger
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.finagle.circeInstances._
import ru.tinkoff.tschema.syntax._

object BinaryModule extends ExampleModule {

  def api =
    post |>
      operation("binary") |>
      binBody("binArr") |>
      $$[String]

  object handler {
    def binary(binArr: Array[Byte]) = new String(binArr, StandardCharsets.UTF_8)
  }

  val swag = MkSwagger(api)
  val route = MkService[Http](api)(handler)
}
