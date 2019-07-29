package ru.tinkoff.tschema
package example

import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.{SwaggerTypeable, _}
import syntax._
import finagle.tethysInstances._

@derive(tethysWriter, openapiParam, httpParam)
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
    tagPrefix('filters) |>
      keyPrefix('echo) |>
      queryParam[Filters]('filt) |>
      get[Filters]

  object handler {
    def echo(filt: Filters) = filt
  }

  val route = MkService[Http](api)(handler)
  val swag  = api.mkSwagger
}
