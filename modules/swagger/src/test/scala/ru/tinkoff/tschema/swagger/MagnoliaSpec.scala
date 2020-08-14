package ru.tinkoff.tschema.swagger
import io.circe.Printer
import ru.tinkoff.tschema.swagger.testDomain.{LotOfVariants, TopStuff, WeirdThing}
import shapeless.test.illTyped
import org.scalatest.flatspec.AnyFlatSpec

class MagnoliaSpec extends AnyFlatSpec {
  import MagnoliaSwagger.{derive => magnoliaDerive}

  "magnolia" should "derive known types" in {
    implicit lazy val weirdThingSwaggerTypeable: SwaggerTypeable[WeirdThing] =
      SwaggerTypeable.make(SwaggerPrimitive.boolean).as[WeirdThing]
    implicit lazy val lots: SwaggerTypeable[LotOfVariants]                   = MagnoliaSwagger.derivedInstance
    lazy val testSwagger: SwaggerTypeable[TopStuff]                          = magnoliaDerive
  }

  it should "not derive unknown types" in {
    illTyped("""lazy val testSwagger: SwaggerTypeable[TopStuff] = magnoliaDerive""")
  }
}

object MagnoliaSpec {

  implicit val cfg: SwaggerTypeable.Config =
    SwaggerTypeable.defaultConfig.snakeCaseProps.snakeCaseAlts.withDiscriminator("type")
  implicit val printer                     = Printer.spaces4.copy(dropNullValues = true)
}
