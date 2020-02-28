package ru.tinkoff.tschema.example

import cats.Monad
import com.twitter.finagle.http.Response
import derevo.derive
import derevo.tethys.{tethysReader, tethysWriter}
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.finagle.Authorization.{Basic, Bearer}
import ru.tinkoff.tschema.finagle.Credentials.secure_equals
import ru.tinkoff.tschema.finagle.util.Unapply
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.swagger.{SwaggerBuilder, _}
import ru.tinkoff.tschema.syntax._

object Authorize {

  final case class User(name: String, roles: List[String])

  @derive(tethysWriter, tethysReader, Swagger)
  final case class Client(name: String, products: List[String])

  val anonClient = Client("anon", List.empty)

  val users = Unapply(
    Map(
      "admin" -> ("adminadmin", List("admin")),
      "guest" -> ("guest", List())
    )
  )

  val clients = Unapply(
    Map(
      "123456" -> Client("client", List("diamond card", "premium subscription"))
    )
  )

  val sessions = Map(
    "x123" -> List(1, 2, 3),
    "x12" -> List(1, 2),
    "y" -> List.empty
  )

  def api =
    tagPrefix("auth") |> ((
      operation('roles) |> basicAuth[User]("users", 'user) |> get |> json[List[String]]
    ) <> (
      operation('client) |> bearerAuth[Option[Client]]("clients", 'client) |> get |> json[Client]
    ) <> (
      operation('numbers) |> apiKeyAuth('sessionId, queryParam[Option[String]]('sessionId)) |> get |> json[List[Int]]
    ))

  object handler {
    def roles(user:        User): List[String] = user.roles
    def client(client:     Option[Client]): Client = client.getOrElse(anonClient)
    def numbers(sessionId: Option[String]): List[Int] = sessionId.flatMap(sessions.get).getOrElse(List(-1))
  }

  implicit def userAuth[H[_]: Monad: Routed]: Authorization[Basic, H, User] = SimpleAuth {
    case Credentials(id @ users(pass, roles), pwd) if secure_equals(pass, pwd) => User(id, roles)
  }

  implicit def clientAuth[H[_]: Monad: Routed]: Authorization[Bearer, H, Client] = SimpleAuth {
    case BearerToken(clients(client)) => client
  }
}

class Authorize[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import Authorize._
  override def route: H[Response] = MkService[H](api)(handler)
  override def swag: SwaggerBuilder = MkSwagger(api)

}
