package ru.tinkoff.tschema.swagger

import cats.syntax.option._
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec

class ResponseHeaderSpec extends AnyFlatSpec {
  "response headers" should "satisfy OpenApi specification" in {
    val urlPattern = """https:\/\/(?:.+)?"""

    val json = OpenApiResponse(
      description = "Redirection".some,
      headers = Map(
        "Location" -> OpenApiHeader(
          "Indicates the URL to redirect a page".some,
          new SwaggerPrimitive(SwaggerStringValue(pattern = urlPattern.some)).some
        )
      )
    ).asJson

    val locationSchema = json.hcursor.downField("headers").downField("Location").downField("schema")

    assert(locationSchema.get[String]("pattern") == Right(urlPattern))

    assert(locationSchema.get[String]("type") == Right("string"))
  }
}
