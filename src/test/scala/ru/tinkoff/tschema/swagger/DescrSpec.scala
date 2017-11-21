package ru.tinkoff.tschema.swagger

import org.scalatest.FlatSpec
import ru.tinkoff.tschema.swagger.DescrSpec.Foo
import shapeless.syntax.singleton._
import shapeless.test.illTyped

class DescrSpec extends FlatSpec {
  "descr" should "set description for existing field" in assert(
    SwaggerTypeable
    .deriveNamedTypeable[Foo]
      .descr('x ->> "foo")
      .typ.asInstanceOf[SwaggerRef]
      .typ.value.asInstanceOf[SwaggerObject]
      .properties.map(prop => (prop.name, prop.description, prop.typ.value)) ===
    Vector(("x", Some("foo"), SwaggerPrimitive.string),
           ("y", None, SwaggerPrimitive.integer))
  )

  it should " not compile if for nonexisting field" in {
    illTyped("""SwaggerTypeable.deriveNamedTypeable[Foo].descr('z ->> "bar")""")
  }
}

object DescrSpec {
  case class Foo(x: String, y: Int)
}
