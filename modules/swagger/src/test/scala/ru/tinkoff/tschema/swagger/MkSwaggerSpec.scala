package ru.tinkoff.tschema.swagger

import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.swagger.PathDescription.DescriptionMap
import ru.tinkoff.tschema.swagger.PathDescription.Target.Tag
import ru.tinkoff.tschema.syntax._

class MkSwaggerSpec extends AnyFlatSpec {
  "tags order in OpenAPI" should "depends of tags order in DSL" in {
    val foo = MkSwagger(
      (
        tag("first") :>
          get :>
          operation("first") :>
          $$[String]
      ) <|> (
        tag("second") :>
          get :>
          operation("second") :>
          $$[String]
      )
    )

    val bar = MkSwagger(
      (
        tag("third") :>
          get :>
          operation("third") :>
          $$[String]
      ) <|> (
        tag("fourth") :>
          get :>
          operation("fourth") :>
          $$[String]
      ) <|> (
        tag("fifth") :>
          get :>
          operation("fifth") :>
          $$[String]
      )
    )

    val description: DescriptionMap = {
      case Tag("first")  => Some("Description of first tag")
      case Tag("second") => Some("Description of second tag")
      case Tag("third")  => Some("Description of third tag")
      case Tag("fourth") => Some("Description of fourth tag")
      case Tag("fifth")  => Some("Description of fifth tag")
      case _             => None
    }

    val output: Vector[OpenApiTag] = Vector(
      OpenApiTag("first", Some("Description of first tag")),
      OpenApiTag("second", Some("Description of second tag")),
      OpenApiTag("third", Some("Description of third tag")),
      OpenApiTag("fourth", Some("Description of fourth tag")),
      OpenApiTag("fifth", Some("Description of fifth tag"))
    )

    assert((foo ++ bar).describe(description).make(OpenApiInfo()).tags == output)
  }

  "optional header" should "has required = false in OpenAPI" in {
    val swagger = MkSwagger(
      (
        tag("tag") :>
          get :>
          operation("operation") :>
          header[Option[String]]("X-Kek-Id") :>
          $$[String]
      )
    )

    assert(!swagger.paths.head.op.parameters.find(_.name == "X-Kek-Id").get.required)
  }
}
