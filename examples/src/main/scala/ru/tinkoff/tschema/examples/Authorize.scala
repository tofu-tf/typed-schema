package ru.tinkoff.tschema.examples

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.Credentials
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport.marshaller
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.akkaHttp.auth.{BasicAuthenticator, BearerAuthenticator}
import ru.tinkoff.tschema.swagger.{SwaggerBuilder, _}
import syntax._
import ru.tinkoff.tschema.typeDSL.QueryParam
import shapeless.{HNil, Witness}

object Authorize extends ExampleModule {
//  implicitly[Serve[QueryParam[Witness.`'sessionId`.T, Option[String]], HNil]]

//  Serve.queryParamServe[Witness.`'sessionId`.T, Option[String], HNil]
  Serve.queryParamServe[Witness.`'sessionId`.T, String, HNil]
  override def route: Route         = MkRoute(api)(handler)
  override def swag: SwaggerBuilder = MkSwagger(api)(())

  final case class User(name: String, roles: List[String])

  @derive(encoder, decoder, swagger)
  final case class Client(name: String, products: List[String])

  val anonClient = Client("anon", List.empty)

  val users = Map(
    "admin" -> ("adminadmin", List("admin")),
    "guest" -> ("guest", List())
  )

  val clients = Map(
    "123456" -> Client("client", List("diamond card", "premium subscription"))
  )

  val sessions = Map(
    "x123" -> List(1, 2, 3),
    "x12"  -> List(1, 2),
    "y"    -> List.empty
  )

  implicit val userAuth: BasicAuthenticator[User] = BasicAuthenticator.of {
    case Credentials.Missing => None
    case cred @ Credentials.Provided(id) =>
      for ((pass, roles) <- users.get(id) if cred.verify(pass)) yield User(id, roles)
  }

  implicit val clientAuth: BearerAuthenticator[Client] = BearerAuthenticator.of {
    case Credentials.Missing             => None
    case cred @ Credentials.Provided(id) => clients.get(id)
  }

  def api =
    tagPrefix('auth) |> ((
      operation('roles) |> basicAuth[User]("users", 'user) |> get[List[String]]
    ) <> (
      operation('client) |> bearerAuth[Option[Client]]("clients", 'client) |> get[Client]
    ) <> (
      operation('numbers) |> apiKeyAuth('sessionId, queryParam[Option[String]]('sessionId)) |> get[List[Int]]
    ))

  object handler {
    def roles(user: User): List[String]               = user.roles
    def client(client: Option[Client]): Client        = client.getOrElse(anonClient)
    def numbers(sessionId: Option[String]): List[Int] = sessionId.flatMap(sessions.get).getOrElse(List(-1))
  }
}
