package tschema
package swagger

import syntax._
import SwaggerTypeable.deriveNamedTypeable
import io.circe.syntax._
import cats.syntax.option._
import io.circe.Printer
import SwaggerXMLOptions.{apply => xmlOpts}
import shapeless.syntax.singleton._
import org.scalatest.wordspec.AnyWordSpec

class XmlSpec extends AnyWordSpec {
  val swaggerJson = XmlSpec.swagger.make(OpenApiInfo()).asJson
  val top = swaggerJson.hcursor
  val method = top.downField("paths").downField("/xml").downField("get")
  val response = method.downField("responses").downField("200").downField("content")
  val bookType = top.downField("components").downField("schemas").downField("Book")
  "Swagger Json" should {
    "contain XML method" in assert(method.succeeded)
    "contain XML media type answer" in assert(
      response.downField("application/xml").downField("schema").downField("$ref").as[String] === Right("#/components/schemas/Book"))
    "contain only one media type" in assert(
      response.keys.toSeq.flatten.length === 1
    )
    "have Book type" in assert(bookType.succeeded)
    "have Book xml-name" in assert(bookType.downField("xml").downField("name").as[String] === Right("book"))
    "have id xml-attribute" in assert(
      bookType.downField("properties").downField("id").downField("xml").downField("attribute").as[Boolean] === Right(true))
    "have tag wrapped array property" in assert(
      bookType.downField("properties").downField("tags").downField("xml").as[SwaggerXMLOptions] === Right(xmlOpts(name = "tag".some, wrapped = true))
    )
  }

}


object XmlSpec {
  case class Book(id: Int, author: String, title: String, tags: List[String])
  implicit val bookSwagger: SwaggerTypeable[Book] =
    deriveNamedTypeable[Book]
      .xmlFld('id ->> xmlOpts(attribute = true))
      .xmlFields("tags" -> xmlOpts(name = "tag".some, wrapped = true))
      .xml(name = "book".some)

  def api = prefix("xml") :> get[Book]

  val swagger = api.mkSwagger
}