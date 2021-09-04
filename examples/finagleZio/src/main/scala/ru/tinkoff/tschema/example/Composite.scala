package ru.tinkoff.tschema
package example
import derevo.cats.show
import derevo.derive
import derevo.tethys.tethysWriter
import ru.tinkoff.tschema.custom.{AsResponse, PlainResult}
import ru.tinkoff.tschema.custom.derivation.{jsonError, plainError}
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.finagle.{NoneCompleting, StringCompleting}
import ru.tinkoff.tschema.swagger.SwaggerContent
import ru.tinkoff.tschema.swagger.Swagger
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.MkSwagger
import syntax._
import zio.ZIO
import AsResponse.Error
import Example.storage
@derive(SwaggerContent, Error)
sealed trait KeySearching

@derive(Swagger, show, plainError(404))
case class NotFound() extends KeySearching

@derive(Swagger, tethysWriter, jsonError(400))
final case class BadKey(s: String) extends KeySearching

object ReceiveModule extends ExampleModule {

  def api =
    tagPrefix("storage") |> queryParam[String]("key") |> ((
      opPut |> body[String]("value") |> plain[Unit]
    ) <> (
      opGet |> jsonErr[KeySearching, String]
    ) <> (
      get |> operation("read") |> plainErr[None.type, String]
    ))

  PlainResult.plainFComplete[Http, Example, Unit]
  def route = MkService[Http](api)(ReceiveService)
  def swag  = MkSwagger(api)
}

object ReceiveService {
  def put(key: String, value: String): Example[Unit]          = storage.flatMap(_.update(_ + (key -> value)))
  def get(key: String): Example[Either[KeySearching, String]] =
    (if (key.isEmpty || key.startsWith("bad")) ZIO.fail(BadKey(key))
     else read(key).some.orElseFail(NotFound())).either
  def read(key: String): Example[Option[String]]              = storage.flatMap(_.get.map(_.get(key)))
}
