package ru.tinkoff.tschema.examples

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Directives.{complete, get, pathPrefix, _}
import akka.stream.ActorMaterializer
import cats.instances.list._
import cats.syntax.foldable._
import ru.tinkoff.tschema.swagger.OpenApiInfo
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._

object TestServer {

  implicit val system = ActorSystem()
  implicit val mat = ActorMaterializer()
  import system.dispatcher

  val modules = List[ExampleModule](TestModule, VersionModule, FiltersModule).combineAll


  val route =
    pathPrefix("api") {
      modules.route
    } ~
      path("swagger")(get(
        complete(modules.swagger.make(OpenApiInfo()).addServer("/api"))
      )) ~
      pathPrefix("webjars")(getFromResourceDirectory("META-INF/resources/webjars")) ~
      path("swagger.php")(
        complete(
          HttpResponse(
            entity = HttpEntity(
              contentType = ContentTypes.`text/html(UTF-8)`,
              string = SwaggerIndex.index.render)))
      )
  def main(args: Array[String]): Unit = {
    for (_ <- Http().bindAndHandle(route, "localhost", 8081))
      println("server started at http://localhost:8081/swagger.php")
  }

}
