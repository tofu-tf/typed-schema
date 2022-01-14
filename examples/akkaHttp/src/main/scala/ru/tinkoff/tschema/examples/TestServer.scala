package ru.tinkoff.tschema.examples

import java.util.Locale

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Directives.{complete, get, pathPrefix, _}
import akka.stream.ActorMaterializer
import cats.instances.list._
import cats.syntax.foldable._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.Printer
import ru.tinkoff.tschema.swagger.{OpenApiInfo, PathDescription}

object TestServer {

  implicit val system = ActorSystem()
  implicit val mat    = ActorMaterializer()
  import system.dispatcher

  val descriptions =
    PathDescription.utf8I18n("swagger", Locale.forLanguageTag("ru"))

  val modules = List[ExampleModule](
    TestModule,
    VersionModule,
    FiltersModule,
    FormFieldsModule,
    Authorize,
    CustomAuth,
    MultiParameters,
    ProxyModule
  ).combineAll

  private[this] implicit val printer: Printer =
    Printer.noSpaces.copy(dropNullValues = true)

  val route                           =
    pathPrefix("api") {
      modules.route
    } ~
      path("swagger")(
        get(
          complete(
            modules.swag
              .describe(descriptions)
              .make(OpenApiInfo())
              .addServer("/api")
          )
        )
      ) ~
      pathPrefix("webjars")(
        getFromResourceDirectory("META-INF/resources/webjars")
      ) ~
      path("swagger.php")(
        complete(
          HttpResponse(
            entity = HttpEntity(
              contentType = ContentTypes.`text/html(UTF-8)`,
              string = SwaggerIndex.index.render
            )
          )
        )
      )
  def main(args: Array[String]): Unit = {
    for (_ <- Http().bindAndHandle(route, "localhost", 8081))
      println("server started at http://localhost:8081/swagger.php")
  }

}
