package ru.tinkoff.tschema
package examples
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsRejected
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Route}
import akka.http.scaladsl.server.directives.Credentials
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import derevo.circe.{decoder, encoder}
import derevo.derive
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.examples.SpecialAuth.validateAuth
import ru.tinkoff.tschema.swagger.{SwaggerOps, SwaggerMapper}
import ru.tinkoff.tschema.swagger._
import syntax._
import ru.tinkoff.tschema.typeDSL.{DSLAtom, DSLAtomAuth}
import shapeless.ops.record.Selector
import shapeless.{HList, Witness => W}

import scala.collection.concurrent.TrieMap

object CustomAuth extends ExampleModule {
  override def route: Route         = MkRoute(api)(handler)
  override def swag: SwaggerBuilder = MkSwagger(api)

  implicit val auth = AuthMap(Map("kriwda" -> (true, "admin"), "oleg" -> (false, "notadmin")))

  def api            =
    tagPrefix("adminka") |> queryParam[String]("userId") |>
      ((
        post |> validateAuth("userId", true) |> body[BanUser]("banUser") |> operation("ban") |> $$[Result]
      ) <> (
        get |> validateAuth("userId", false) |> operation("bans") |> $$[List[BanUser]]
      ))

  private val banned = TrieMap.empty[String, BanUser]

  object handler {
    def ban(banUser: BanUser, userId: String): Result = {
      banned.put(banUser.userToBan, banUser)
      Result(s"$userId ok")
    }
    def bans: List[BanUser]                           = banned.values.toList
  }
}

@derive(encoder, decoder, Swagger)
final case class BanUser(userToBan: String, description: Option[String], ttl: Option[Long])

@derive(encoder, decoder, Swagger)
final case class Result(message: String)

final case class AuthMap(values: Map[String, (Boolean, String)]) {
  def get(key: String, admin: Boolean): Option[String] =
    values.get(key).collect {
      case (adm, secret) if adm || (!admin) => secret
    }
}

class SpecialAuth[userVar, admin <: Boolean] extends DSLAtom

object SpecialAuth {
  def validateAuth[userVar, admin <: Boolean](
      userVar: W.Aux[userVar],
      admin: W.Aux[admin]
  ): SpecialAuth[userVar, admin] =
    new SpecialAuth

  import Serve.{Check, serveReadCheck}

  implicit def swagger[userVar, admin <: Boolean]: SwaggerMapper[SpecialAuth[userVar, admin]] =
    bearerAuth[String]("kriwda", "kriwda").swaggerMapper.as[SpecialAuth[userVar, admin]]

  import akka.http.scaladsl.server.Directives._

  implicit def serve[In <: HList, userVar, admin <: Boolean](implicit
      auth: AuthMap,
      admin: W.Aux[admin],
      select: Selector.Aux[In, userVar, String]
  ): Check[SpecialAuth[userVar, admin], In] =
    serveReadCheck[SpecialAuth[userVar, admin], userVar, String, In](userId =>
      authenticateOAuth2PF(
        "kriwda",
        {
          case cred @ Credentials.Provided(_) if auth.get(userId, admin.value).exists(cred.verify) => userId
        }
      ).tmap(_ => ())
    )
}
