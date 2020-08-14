package ru.tinkoff.tschema.swagger

import java.util.ResourceBundle

import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.typeDSL.Complete
import ru.tinkoff.tschema.syntax._
import PathDescriptionSpec.api
import io.circe.Printer
import io.circe.syntax._
import ru.tinkoff.tschema.swagger.OpenApi.Method

class PathDescriptionSpec extends AnyFlatSpec {
  val bundle  = ResourceBundle.getBundle("swagger")
  val swagger = MkSwagger(api).describe(PathDescription.i18n(bundle)).make()

  "description" should "read descriptions from bundle" in {
    val path = swagger.paths("/test/check")(Method.get)
    assert(path.responses.codes(200).description.get == "Good")
    assert(path.responses.codes(400).description.get == "Bad")
    assert(path.responses.codes(500).description.get == "Ugly")
  }
}

object PathDescriptionSpec {
  class GoodBadUgly[X]

  def gbu[A]: Complete[GoodBadUgly[A]] = new Complete

  implicit def gbuSwagger[X: Swagger]: MkSwagger[Complete[GoodBadUgly[X]]] =
    MkSwagger
      .summon[Complete[X]]
      .addResponse(400)
      .addResponse(500)
      .as

  val api =
    tagPrefix("test") |> operation("check") |> get |> gbu[String]
}
