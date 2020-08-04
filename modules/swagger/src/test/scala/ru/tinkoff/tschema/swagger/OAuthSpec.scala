package ru.tinkoff.tschema.swagger

import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.syntax._

class OAuthSpec extends AnyFlatSpec {
  val swagger = MkSwagger(OAuthSpec.api).make()
  val schemes = swagger.components.securitySchemes

  "securitySchemes" should "have a realm with specified name" in {
    assert(schemes.contains(OAuthSpec.realm))
  }

  "realm" should "contain authorizationCode flow" in {
    val flows = schemes(OAuthSpec.realm).flows
    assert(flows.contains("authorizationCode"))
  }

  "flow" should "contain correct data" in {
    val flows = schemes(OAuthSpec.realm).flows
    assert(flows("authorizationCode") == OAuthSpec.authFlow)
  }
}

object OAuthSpec {
  val authFlow = OpenApiFlow.authorizationCode(
    authorizationUrl = "http://auth.url",
    tokenUrl = "http://token.url"
  )

  val realm = "realmName"

  val config: OAuthConfig =
    OAuthConfig(realm).flow(authFlow)
  val api                 = operation("test") |> get |> oauth[From, To]("name", config) |> $$[String]

  trait From

  trait To
}
