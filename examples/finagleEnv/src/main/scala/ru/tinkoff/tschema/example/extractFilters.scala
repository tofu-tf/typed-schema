package ru.tinkoff.tschema
package example

import cats.Monad
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.finagle.{MkService, RoutedPlus}
import ru.tinkoff.tschema.swagger.{SwaggerTypeable, _}
import syntax._
import finagle.tethysInstances._
import ru.tinkoff.tschema.example.FiltersModule.{api, handler}

@derive(tethysWriter, openapiParam, httpParam)
case class Filters(foo: Option[String], bar: Option[Int])

object Filters {
  implicit def swagger: SwaggerTypeable[Filters] =
    SwaggerTypeable
      .deriveNamedTypeable[Filters]
      .describe("filter options")
      .describeFields("foo" -> "filter for foo", "bar" -> "fitler for bar")
}

class FiltersModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  implicit val printer = io.circe.Printer.noSpaces.copy(dropNullValues = true)
  val route = MkService[H](api)(handler)
  val swag = api.mkSwagger
}

object FiltersModule {
  def api =
    tagPrefix('filters) |>
      keyPrefix('echo) |>
      queryParam[Filters]('filt) |>
      get[Filters]

  object handler {
    def echo(filt: Filters) = filt
  }

}
