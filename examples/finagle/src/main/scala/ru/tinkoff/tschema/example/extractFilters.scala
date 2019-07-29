package ru.tinkoff.tschema
package example

import cats.syntax.apply._
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import ru.tinkoff.tschema.finagle.{MkService, Routed, Serve}
import Routed.uriParam
import ru.tinkoff.tschema.swagger.{SwaggerTypeable, _}
import ru.tinkoff.tschema.typeDSL.DSLAtom
import shapeless.{HList, Witness}
import syntax._
import finagle.tethysInstances._
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances._

@derive(tethysWriter, openapiParam)
case class Filters(foo: Option[String], bar: Option[Int])

object Filters {
  implicit def swagger: SwaggerTypeable[Filters] =
    SwaggerTypeable
      .deriveNamedTypeable[Filters]
      .describe("filter options")
      .describeFields("foo" -> "filter for foo", "bar" -> "fitler for bar")
}

final class extractFilters[name] extends DSLAtom

object extractFilters {
  import ExampleEnv.{httpMonad, httpRouted}

  def apply[name](w: Witness.Lt[name]): extractFilters[name] = new extractFilters

  implicit def filtersSwagger[name] = (queryParam[Filters]('filters)).swaggerMapper.as[extractFilters[name]]

  implicit def filtersServe[name <: Symbol, In <: HList] =
    Serve.add[extractFilters[name], Http, In, Filters, name](
      (uriParam[Http, Option[String]]("foo"), uriParam[Http, Option[Int]]("bar")).mapN(Filters(_, _)))
}

object FiltersModule extends ExampleModule {
  implicit val printer = io.circe.Printer.noSpaces.copy(dropNullValues = true)

  def api = tagPrefix('filters) |> keyPrefix('echo) |> extractFilters('filt) |> get[Filters]
  object handler {
    def echo(filt: Filters) = filt
  }

  val route = MkService[Http](api)(handler)
  val swag  = api.mkSwagger
}
