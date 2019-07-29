package ru.tinkoff.tschema.example

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.example.ExampleEnv.incrementAlohas
import ru.tinkoff.tschema.finagle.{Complete, MkService}
import ru.tinkoff.tschema.syntax._
import zio._
import ru.tinkoff.tschema.finagle.tethysInstances._
import cats.instances.all._
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.swagger.MkSwagger

case object Greeting extends ExampleModule {
  @derive(tethysWriter, swagger)
  final case class Aloha(greeing: String, calledCount: Int)

  def api =
    (operation('hello) |> $$[String]) <>
      (operation('aloha) |> $$[Aloha])

  object handler {
    def hello: Example[String] = ZIO.succeed("Hello")
    def aloha: Example[Aloha] =
      incrementAlohas.flatMap(i => ZIO.succeed(Aloha("aloha", i)))
  }

  val route = MkService[Http](api)(handler)
  val swag = MkSwagger(api)
}