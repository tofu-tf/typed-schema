package ru.tinkoff.tschema.examples

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.swagger.{SwaggerTypeable, _}
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL.DSLAtom
import shapeless.{HList, Witness}

@derive(encoder, decoder)
case class Filters(foo: Option[String], bar: Option[Int])

object Filters {
  implicit def swagger: SwaggerTypeable[Filters] =
    SwaggerTypeable
    .deriveNamedTypeable[Filters]
      .describe("filter options")
      .describeFields("foo" -> "filter for foo",
                      "bar" -> "fitler for bar")
}

final class extractFilters[name] extends DSLAtom

object extractFilters {
  def apply[name](w: Witness.Lt[name]): extractFilters[name] = new extractFilters
  implicit def filtersSwagger[name] =
    (queryParam[Option[String]]('foo) |> queryParam[Option[Int]]('bar)).swaggerMapper.as[extractFilters[name]]

  import akka.http.scaladsl.server.Directives._

  implicit def filtersServe[name <: Symbol, In <: HList] =
    Serve.serveAdd[extractFilters[name], In, Filters, name] {
      parameters('foo.?, 'bar.as[Int].?).tmap { case (foo, bar) => Tuple1(Filters(foo, bar)) }
    }
}

object FiltersModule extends ExampleModule {
 implicit val printer = io.circe.Printer.noSpaces.copy(dropNullValues = true)

  def api = tagPrefix('filters) |> keyPrefix('echo) |> extractFilters('filt) |> get[Filters]
  object handler {
    def echo(filt: Filters) = filt
  }


  val route = MkRoute(api)(handler)
  val swag = api.mkSwagger
}
