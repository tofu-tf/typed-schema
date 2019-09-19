package ru.tinkoff.tschema.swagger
import io.circe.Printer
import org.scalatest.FlatSpec
import ru.tinkoff.tschema.swagger.MagnoliaSpec._
import shapeless.test.illTyped

class MagnoliaSpec extends FlatSpec {
  import MagnoliaSwagger.{derive => magnoliaDerive}

  "magnolia" should "derive known types" in {
    implicit lazy val weirdThingSwaggerTypeable: SwaggerTypeable[WeirdThing] =
      SwaggerTypeable.make(SwaggerPrimitive.boolean).as[WeirdThing]
    implicit lazy val lots: SwaggerTypeable[LotOfVariants] = MagnoliaSwagger.derivedInstance
    lazy val testSwagger: SwaggerTypeable[TopStuff] = magnoliaDerive
  }

  it should "not derive unknown types" in {
    illTyped("""lazy val testSwagger: SwaggerTypeable[TopStuff] = magnoliaDerive""")
  }
}

object MagnoliaSpec {
  trait WeirdThing

  final case class InnerStuff(shouldFind: String, shouldNotFind: WeirdThing, outer: Option[OuterStuff])

  sealed trait LotOfVariants

  object LotOfVariants
  final case class OuterStuff(foo: Int, inner: List[InnerStuff]) extends LotOfVariants
  case object AnotherLol extends LotOfVariants

  final case class TopStuff(vars: LotOfVariants) extends LotOfVariants

  implicit val cfg: SwaggerTypeable.Config = SwaggerTypeable.defaultConfig.snakeCaseProps.snakeCaseAlts.withDiscriminator("type")
  implicit val printer = Printer.spaces4.copy(dropNullValues = true)
}
