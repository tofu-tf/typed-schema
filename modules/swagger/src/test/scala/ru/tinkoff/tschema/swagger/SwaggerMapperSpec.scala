package ru.tinkoff.tschema.swagger

import derevo.derive
import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.typeDSL._
import shapeless.Witness

@derive(Swagger)
case class Enterprise(market: String, product: Int)

class SwaggerMapperSpec extends AnyFlatSpec {
  "type schemas" should "be the same for all types of params" in {
    type name = Witness.`""`.T
    val query          = implicitly[SwaggerMapper[QueryParam[name, Enterprise]]].types
    val formField      = implicitly[SwaggerMapper[FormField[name, Enterprise]]].types
    val multipartField = implicitly[SwaggerMapper[MultipartField[name, Enterprise]]].types
    val header         = implicitly[SwaggerMapper[Header[name, Enterprise]]].types
    val cookie         = implicitly[SwaggerMapper[Cookie[name, Enterprise]]].types
    val capture        = implicitly[SwaggerMapper[Capture[name, Enterprise]]].types
    val reqBody        = implicitly[SwaggerMapper[ReqBody[name, Enterprise]]].types
    assert(query == formField)
    assert(formField == header)
    assert(header == cookie)
    assert(cookie == capture)
    assert(capture == reqBody)
  }
}
