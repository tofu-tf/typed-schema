package ru.tinkoff.tschema.example

import java.util.Locale

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.example.Server.{getClass, modules}
import ru.tinkoff.tschema.examples.SwaggerIndex
import ru.tinkoff.tschema.finagle.{Rejection, Routed}
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.finagle.envRouting.Rejected
import ru.tinkoff.tschema.swagger.{OpenApiInfo, PathDescription}
import io.circe.syntax._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import io.circe.Printer
import monix.eval.Task
import tofu.env.Env

object ExampleSwagger {
  private implicit val printer: Printer = Printer.spaces2.copy(dropNullValues = true)

  private val swaggerHttp: Http[Response]            = {
    val response = message.stringResponse(SwaggerIndex.index.render)
    response.setContentType("text/html(UTF-8)")
    Routed.checkPath[Http, Response]("/swagger.php", Http.pure(response))
  }

  private def resource(name: String): Http[Response] =
    Env.fromTask(
      Task.delay {
        val BufSize         = 1024
        val response        = Response()
        val stream          = getClass.getResourceAsStream(name)
        val arr             = Array.ofDim[Byte](BufSize)
        def readAll(): Unit =
          stream.read(arr) match {
            case BufSize          =>
              response.write(arr)
              readAll()
            case size if size > 0 =>
              response.write(arr.slice(0, size))
              readAll()
            case _                =>
          }
        readAll()
        response
      }.executeOn(resources)
        .onErrorHandleWith(_ => Task.raiseError(Rejected(Rejection.notFound)))
    )

  private val swaggerResources: Http[Response]       =
    Routed.path[Http].map(_.toString).flatMap {
      case s if s.startsWith("/webjars") => resource("/META-INF/resources" + s)
      case _                             => Routed.reject[Http, Response](Rejection.notFound)
    }

  private val swaggerJson: Http[Response]            = {
    val swagger      = modules.foldMap(_.swag)
    val descriptions =
      PathDescription.utf8I18n("swagger", Locale.forLanguageTag("ru"))
    val json         = swagger.describe(descriptions).make(OpenApiInfo()).asJson.pretty(printer)
    val response     = message.jsonResponse(json)
    Routed.checkPath[Http, Response]("/swagger", Env.pure(response))
  }

  val route: Http[Response] = swaggerResources <+> swaggerHttp <+> swaggerJson
}
