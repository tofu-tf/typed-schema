package ru.tinkoff.tschema.example

import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import derevo.derive
import derevo.tethys.tethysWriter
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.example.Greeting.Aloha
import ru.tinkoff.tschema.finagle.{LiftHttp, MkService, RoutedPlus}
import ru.tinkoff.tschema.swagger.{MkSwagger, Swagger}
import ru.tinkoff.tschema.syntax._


class Greeting[H[_]: Monad: RoutedPlus: LiftHttp[*[_], F], F[_]: Monad: Alohas] extends ExampleModule[H] {

  def api =
    (operation('hello) |> plain[String]) <>
      (operation('aloha) |> json[Aloha])

  val route = MkService[H](api)(new Greeting.handler[F])
  val swag = MkSwagger(api)
}

object Greeting {
  @derive(tethysWriter, Swagger)
  final case class Aloha(greeing: String, calledCount: Int)

  class handler[F[_]: Applicative: Alohas] {
    def hello: F[String] = "Hello".pure[F]
    def aloha: F[Aloha] = Alohas[F].increment().map(i => Aloha("aloha", i))
  }
}
