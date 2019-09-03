package ru.tinkoff.tschema.example

import cats.{Applicative, Monad}
import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.finagle.{Complete, CompleteIn, LiftHttp, MkService, RoutedPlus}
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.finagle.tethysInstances._
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.functor._
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.example.Greeting.Aloha
import ru.tinkoff.tschema.swagger.MkSwagger
import shapeless.HNil

class Greeting[H[_]: Monad: RoutedPlus: LiftHttp[*[_], F], F[_]: Monad: Alohas] extends ExampleModule[H] {

  def api =
    (operation('hello) |> $$[String]) <>
      (operation('aloha) |> $$[Aloha])

  val route = MkService[H](api)(new Greeting.handler[F])
  val swag = MkSwagger(api)
}

object Greeting {
  @derive(tethysWriter, swagger)
  final case class Aloha(greeing: String, calledCount: Int)

  class handler[F[_]: Applicative: Alohas] {
    def hello: F[String] = "Hello".pure[F]
    def aloha: F[Aloha] = Alohas[F].increment().map(i => Aloha("aloha", i))
  }
}
