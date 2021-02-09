package ru.tinkoff.tschema.swagger.typesafe

import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.typeDSL.Complete
import ru.tinkoff.tschema.syntax._
import TypesafeDescriptionSpec.api
import com.typesafe.config.{ConfigFactory => TypesafeConfigFactory}
import derevo.derive
import ru.tinkoff.tschema.swagger.{MkSwagger, Swagger, SwaggerObject}
import ru.tinkoff.tschema.swagger.OpenApi.Method

class TypesafeDescriptionSpec extends AnyFlatSpec {
  val swagger = MkSwagger(api).describe(TypesafeDescription(TypesafeConfigFactory.load("swagger.conf"))).make()

  "description" should "read descriptions from typesafe config" in {
    val path = swagger.paths("/test/check/{mark}")(Method.get)
    assert(path.responses.codes(200).description.get == "Good")
    assert(path.responses.codes(400).description.get == "Bad")
    assert(path.responses.codes(500).description.get == "Ugly")
    assert(path.parameters.head.description.get == "Mark")

    val check = swagger.paths("/check")(Method.get)
    assert(swagger.tags.head.description.get == "Good test")
    assert(
      swagger.components
        .schemas("ru.tinkoff.tschema.swagger.typesafe.TypesafeDescriptionSpec.TestResponse")
        .typ
        .asInstanceOf[SwaggerObject]
        .properties
        .map(_.description.get) == Vector("Response status", "Title")
    )
    assert(check.summary.get == "Check swagger description")
  }
}

object TypesafeDescriptionSpec {
  @derive(Swagger)
  case class TestResponse(status: String, title: String)

  class GoodBadUgly[X]

  def gbu[A]: Complete[GoodBadUgly[A]] = new Complete

  implicit def gbuSwagger[X: Swagger]: MkSwagger[Complete[GoodBadUgly[X]]] =
    MkSwagger
      .summon[Complete[X]]
      .addResponse(400)
      .addResponse(500)
      .as

  val api =
    (tagPrefix("test") |> operation("check") |> capture[String]("mark") |> get |> gbu[String]) <|>
      (tag("test2") |> operation("check") |> get |> $$[TestResponse])
}
