package ru.tinkoff.testApi

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import io.circe.generic.JsonCodec
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.swagger.SwaggerTypeable
import ru.tinkoff.tschema.typeDSL.DSLAtom
import shapeless.{HList, Witness}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.testApi.VersionApp.route

@JsonCodec
case class Filters(foo: Option[String], bar: Option[Int])

object Filters {
  implicit def swagger: SwaggerTypeable[Filters] = SwaggerTypeable.deriveNamedTypeable[Filters]
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

object FiltersApp extends App {
  def api = keyPrefix('echo) |> extractFilters('filt) |> get[Filters]
  object handler {
    def echo(filt: Filters) = filt
  }


  implicit val system = ActorSystem()
  implicit val mat = ActorMaterializer()

  import akka.http.scaladsl.server.Directives._
  import system.dispatcher

  val route = pathPrefix("api") { MkRoute(api)(handler) } ~
              pathPrefix("swagger")(get(
                complete(api.mkSwagger.make(OpenApiInfo()))
              ))

  for (_ <- Http().bindAndHandle(route, "localhost", 8081))
    println("server started at http://localhost:8081")
}
