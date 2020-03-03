package ru.tinkoff.tschema
package example

import cats.Monad
import derevo.derive
import derevo.tethys.tethysWriter
import ru.tinkoff.tschema.finagle.RoutedPlus
import tschema.finagle.MkService
import tschema.swagger._
import tschema.syntax._
import ru.tinkoff.tschema.example.FiltersModule.{api, handler}
import ru.tinkoff.tschema.param.HttpParam
import ru.tinkoff.tschema.swagger.AsOpenApiParam
import tschema.custom.syntax._

@derive(tethysWriter, AsOpenApiParam, HttpParam)
case class Filters(foo: Option[String], bar: Option[Int])

object Filters {
  implicit def swagger: Swagger[Filters] =
    Swagger
      .instance[Filters]
      .describe("filter options")
      .describeFields("foo" -> "filter for foo", "bar" -> "fitler for bar")
}

class FiltersModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  implicit val printer = io.circe.Printer.noSpaces.copy(dropNullValues = true)
  val route = MkService[H](api)(handler)
  val swag = MkSwagger(api)
}

object FiltersModule {
  def api =
    tagPrefix("filters") |>
      keyPrefix("echo") |>
      queryParam[Filters]("filt") |>
      get |> json[Filters]

  object handler {
    def echo(filt: Filters) = filt
  }

}
