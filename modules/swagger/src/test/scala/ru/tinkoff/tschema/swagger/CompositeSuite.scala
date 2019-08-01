package ru.tinkoff.tschema
package swagger
import cats.instances.option._
import io.circe.ACursor
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}
import ru.tinkoff.tschema.swagger.CompositeSuite.composwag
import syntax._

class CompositeSuite extends FlatSpec with Matchers {
  def op(name: String, status: Int) =
    composwag.hcursor.downField("paths").downField(s"/$name").downField("get").downField("responses").downField(status.toString)

  def schemaType(cursor: ACursor) =
    cursor
      .downField("content")
      .downField("application/json")
      .downField("schema")
      .get[String]("type")

  "composite swagger resolution" should "construct optional path" in {

    schemaType(op("optInt", 200)) shouldBe Right("integer")

    op("optInt", 404).get[Map[String, String]]("content") shouldBe Right(Map())
  }

  it should "construct error controlled path" in {
    schemaType(op("stringOrInt", 200)) shouldBe Right("integer")

    schemaType(op("stringOrInt", 400)) shouldBe Right("string")
  }

}

object CompositeSuite {
  val composwag = MkSwagger(api).make(OpenApiInfo()).asJson
  def api =
    get |> (
      (keyPrefix("optInt") |> ($$[Int] ?)) <>
        (keyPrefix("stringOrInt") |> ($$[String] || $$[Int]))
    )
}
