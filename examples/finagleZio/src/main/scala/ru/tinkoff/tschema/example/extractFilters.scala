package ru.tinkoff.tschema
package example

import derevo.derive
import derevo.tethys.tethysWriter
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.param.HttpParam
import ru.tinkoff.tschema.swagger.{SwaggerTypeable, _}
import ru.tinkoff.tschema.custom.syntax._
import syntax._

@derive(tethysWriter, AsOpenApiParam, HttpParam)
case class Filters(foo: Option[String], bar: Option[Int])

object Filters {
  implicit def swagger: SwaggerTypeable[Filters] =
    SwaggerTypeable
      .deriveNamedTypeable[Filters]
      .describe("filter options")
      .describeFields("foo" -> "filter for foo", "bar" -> "fitler for bar")
}

object FiltersModule extends ExampleModule {
  implicit val printer = io.circe.Printer.noSpaces.copy(dropNullValues = true)

  def api =
    tagPrefix("filters") |>
      keyPrefix("echo") |>
      queryParam[Filters]("filt") |>
      get |>
      json[Filters]

  object handler {
    def echo(filt: Filters) = filt
  }

  val route = MkService[Http](api)(handler)
  val swag  = api.mkSwagger
}
