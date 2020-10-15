package ru.tinkoff.tschema.swagger

import derevo.derive
import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.typeDSL._

@derive(Swagger)
case class Enterprise(market: String, product: Int)

class SwaggerMapperSpec extends AnyFlatSpec {
  "type schemas" should "be the same for all types of params" in {
    val query     = implicitly[SwaggerMapper[QueryParam["", Enterprise]]].types
    val formField = implicitly[SwaggerMapper[FormField["", Enterprise]]].types
    val header    = implicitly[SwaggerMapper[Header["", Enterprise]]].types
    val cookie    = implicitly[SwaggerMapper[Cookie["", Enterprise]]].types
    val capture   = implicitly[SwaggerMapper[Capture["", Enterprise]]].types
    val reqBody   = implicitly[SwaggerMapper[ReqBody["", Enterprise]]].types
    assert(query == formField)
    assert(formField == header)
    assert(header == cookie)
    assert(cookie == capture)
    assert(capture == reqBody)
  }
}
