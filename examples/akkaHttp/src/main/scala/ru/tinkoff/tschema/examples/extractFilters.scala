package ru.tinkoff.tschema
package examples

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances.{httpParam, openapiParam}
import ru.tinkoff.tschema.akkaHttp.MkRoute
import ru.tinkoff.tschema.swagger.{SwaggerTypeable, _}
import syntax._

@derive(encoder, decoder, httpParam, openapiParam)
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

  val route = MkRoute(api)(handler)
  val swag  = api.mkSwagger
}
