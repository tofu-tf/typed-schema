package ru.tinkoff.tschema.swagger

import cats.Eval
import org.scalatest.flatspec.AnyFlatSpec

class SwaggerTypeableTest extends AnyFlatSpec {

  "SwaggerTypeable.genTypeable" should "derive typeable for case class" in {
    SwaggerTypeable.genTypeable[SwaggerTypeableTest.TestClass].typ match {
      case SwaggerObject(props, required) =>
        val nowProps = props.map(p => p.copy(typ = Eval.now(p.typ.value)))
        assert(nowProps == Vector(
          SwaggerProperty("a", None, Eval.now(SwaggerPrimitive.integer)),
          SwaggerProperty("b", None, Eval.now(SwaggerPrimitive.integer))
        ))

        assert(required.value == Vector("a"))
      case res =>
        fail("Unknown SwaggerType: " + res)
    }
  }
}

object SwaggerTypeableTest {
  case class TestClass(a: Int, b: Option[Int])
}
