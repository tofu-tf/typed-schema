package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.swagger.DescrSpec._
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.tinkoff.tschema.swagger.Swagger

class DescrSpec extends AnyFlatSpec with Matchers {
  "descr" should "set description for existing field" in assert(
    Swagger
      .instance[Foo]
      .descr(Symbol("x") ->> xDescr)
      .typ
      .asInstanceOf[SwaggerRef]
      .typ
      .value
      .asInstanceOf[SwaggerObject]
      .properties
      .map(prop => (prop.name, prop.description, prop.typ.value)) ===
      Vector(("x", Some(xDescr), SwaggerPrimitive.string), ("y", None, SwaggerPrimitive.integer))
  )

  it should " not compile if for nonexisting field" in {
    illTyped("""SwaggerTypeable.deriveNamedTypeable[Foo].descr('z ->> "bar")""")
  }

  val barTyp = SwaggerTypeable.deriveNamedTypeable[Bar].typ

  "DescribeTypeable" should "set description for type" in assert(barTyp match {
    case ref: SwaggerRef => ref.descr.contains(barDescr)
    case _               => false
  })
  it should "set description for fields" in assert(barTyp.deref.value match {
    case obj: SwaggerObject =>
      obj.properties.forall {
        case SwaggerProperty("x", None, _)           => true
        case SwaggerProperty("y", Some(`yDescr`), _) => true
        case _                                       => false
      }
    case _                  => false
  })

}

object DescrSpec {
  final case class Foo(x: String, y: Int)
  final case class Bar(x: String, y: String)
  val barDescr = "Bar type"
  val xDescr   = "IPhone 10 version"
  val yDescr   = "Real men's chromosome"

  implicit val barDescription: DescribeTypeable[Bar] = DescribeTypeable.make[Bar](barDescr, "y" -> yDescr)
}
