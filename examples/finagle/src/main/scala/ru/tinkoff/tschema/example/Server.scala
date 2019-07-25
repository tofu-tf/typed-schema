package ru.tinkoff.tschema.example

import java.util.Locale

import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import com.twitter.finagle.Http
import com.twitter.finagle.http.Response
import com.twitter.util.{Await, Duration}
import io.circe.syntax._
import ru.tinkoff.tschema.examples.SwaggerIndex
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.finagle.zioInstance.Rejected
import ru.tinkoff.tschema.finagle.{Rejection, Routed, Runnable}
import ru.tinkoff.tschema.swagger.{OpenApiInfo, PathDescription}
import zio.blocking._
import zio.console._
import zio.{blocking => _, _}

object Server extends App {

  val modules: List[ExampleModule] = List(Greeting, TestModule)

  val swaggerHttp: Http[Response] = {
    val response = message.stringResponse(SwaggerIndex.index.render)
    response.setContentType("text/html(UTF-8)")
    Routed.checkPath[Http, Response]("/swagger.php", ZIO.succeed(response))
  }

  def resource(name: String): Http[Response] =
    blocking(ZIO {
      val BufSize = 1024
      val response = Response()
      val stream = getClass.getResourceAsStream(name)
      val arr = Array.ofDim[Byte](BufSize)
      def readAll(): Unit =
        stream.read(arr) match {
          case BufSize =>
            response.write(arr)
            readAll()
          case size if size > 0 =>
            response.write(arr.slice(0, size))
            readAll()
          case _ =>
        }
      readAll()
      response
    }).catchAll(_ => ZIO.fail(Rejected(Rejection.notFound)))
      .provideSome(_.embedded)

  val swaggerResources: Http[Response] =
    Routed.path.map(_.toString).flatMap {
      case s if s.startsWith("/webjars") => resource("/META-INF/resources" + s)
      case _                             => Routed.reject[Http, Response](Rejection.notFound)
    }

  val swaggerJson: Http[Response] = {
    val swagger = modules.foldMap(_.swag)
    val descriptions =
      PathDescription.utf8I18n("swagger", Locale.forLanguageTag("ru"))
    val json = swagger.describe(descriptions).make(OpenApiInfo()).asJson.spaces2
    val response = message.jsonResponse(json)
    Routed.checkPath[Http, Response]("/swagger", ZIO.succeed(response))
  }

  val svc: Http[Response] =
    modules.foldMapK(_.route) <+> swaggerHttp <+> swaggerJson <+> swaggerResources

  val server = for {
    srv <- Runnable.run[Example](svc)
    list <- ZIO.effect(Http.serve("0.0.0.0:9191", srv))
    _ <- putStr(s"started at ${list.boundAddress}")
    _ <- ZIO.effect(Await.ready(list, Duration.Top)).fork
    res <- ZIO.never
  } yield res

  def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    for {
      ref <- Ref.make(0)
      _ <- server
        .catchAll(ex => putStr(ex.getMessage))
        .provide(ExampleEnv("lol", ref))
    } yield 0
}
