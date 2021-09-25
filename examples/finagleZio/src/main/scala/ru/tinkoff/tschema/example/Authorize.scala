package ru.tinkoff.tschema.example

import cats.Applicative
import com.twitter.finagle.http.Response
import com.twitter.util.Base64StringEncoder
import derevo.circe.{decoder, encoder}
import derevo.derive
import derevo.tethys.{tethysReader, tethysWriter}
import ru.tinkoff.tschema.finagle.Authorization.{Basic, Bearer, Kind, AuthorizationS}
import ru.tinkoff.tschema.finagle.{Authorization, Credentials, Rejection, Routed, SimpleAuth, BearerToken}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.syntax._
import shapeless.{HNil, Witness}
import cats.syntax.applicative._
import ru.tinkoff.tschema.finagle.Credentials.secure_equals
import ru.tinkoff.tschema.finagle.util.Unapply

import scala.annotation.tailrec
import ru.tinkoff.tschema.finagle.tethysInstances._

object Authorize extends ExampleModule {

  implicitly[Routed[Http]]
  override def route: Http[Response] = MkService[Http](api)(handler)
  override def swag: SwaggerBuilder  = MkSwagger(api)

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

  val sessions                                                  = Map(
    "x123" -> List(1, 2, 3),
    "x12"  -> List(1, 2),
    "y"    -> List.empty
  )

  implicit val userAuth: AuthorizationS[Basic, Http, User]      = SimpleAuth {
    case Credentials(id @ users(pass, roles), pwd) if secure_equals(pass, pwd) => User(id, roles)
  }

  implicit val clientAuth: AuthorizationS[Bearer, Http, Client] = SimpleAuth { case BearerToken(clients(client)) =>
    client
  }

  def api =
    tagPrefix("auth") |> ((
      operation("roles") |> basicAuth[User]("users", "user") |> get[List[String]]
    ) <> (
      operation("client") |> bearerAuth[Option[Client]]("clients", "client") |> get[Client]
    ) <> (
      operation("numbers") |> apiKeyAuth("sessionId", queryParam[Option[String]]("sessionId")) |> get[List[Int]]
    ))

  object handler {
    def roles(user: User): List[String]               = user.roles
    def client(client: Option[Client]): Client        = client.getOrElse(anonClient)
    def numbers(sessionId: Option[String]): List[Int] = sessionId.flatMap(sessions.get).getOrElse(List(-1))
  }
}
