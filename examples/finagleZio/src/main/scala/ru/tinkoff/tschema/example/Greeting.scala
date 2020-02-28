package ru.tinkoff.tschema.example

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.example.Example.incrementAlohas
import ru.tinkoff.tschema.finagle.{Complete, MkService}
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.finagle.tethysInstances._
import cats.instances.all._
import derevo.derive
import derevo.tethys.tethysWriter
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.swagger.{MkSwagger, Swagger}
import zio.ZIO

case object Greeting extends ExampleModule {
  @derive(tethysWriter, Swagger)
  final case class Aloha(greeing: String, calledCount: Int)

  def api =
    (operation('hello) |> plain[String]) <>
      (operation('aloha) |> json[Aloha])

  object handler {
    def hello: Example[String] = ZIO.succeed("Hello")
    def aloha: Example[Aloha] =
      incrementAlohas.flatMap(i => ZIO.succeed(Aloha("aloha", i)))
  }

  val route = MkService[Http](api)(handler)
  val swag = MkSwagger(api)
}