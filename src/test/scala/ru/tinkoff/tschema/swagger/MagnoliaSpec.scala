package ru.tinkoff.tschema.swagger
import java.time.ZonedDateTime

import io.circe.Printer
import io.circe.syntax._
import org.scalatest.FlatSpec
import ru.tinkoff.tschema.swagger.MagnoliaSpec._
import shapeless.test.illTyped

class MagnoliaSpec extends FlatSpec {
  import MagnoliaSwagger.{derive => magnoliaDerive}

  "magnolia" should "derive known types" in {
    implicit lazy val weirdThingSwaggerTypeable: SwaggerTypeable[WeirdThing] =
      SwaggerTypeable.make(SwaggerPrimitive.boolean).as[WeirdThing]
    lazy val testSwagger: SwaggerTypeable[TopShit] = magnoliaDerive
  }

  it should "not derive unknown types" in {
    illTyped("""lazy val testSwagger: SwaggerTypeable[TopShit] = magnoliaDerive""")
  }
}

object MagnoliaSpec {
  trait WeirdThing

  final case class InnerShit(shouldFind: String, shouldNotFind: WeirdThing, outer: Option[OuterShit])

  sealed trait LotOfVariants

  final case class OuterShit(foo: Int, inner: List[InnerShit]) extends LotOfVariants
  case object AnotherLol extends LotOfVariants

  final case class TopShit(vars: LotOfVariants) extends LotOfVariants

  implicit val cfg: SwaggerTypeable.Config = SwaggerTypeable.defaultConfig.snakeCaseProps.snakeCaseAlts.withDiscriminator("type")
  implicit val printer = Printer.spaces4.copy(dropNullValues = true)

  def main(args: Array[String]): Unit = {
    import MagnoliaSwagger.{derive => magnoliaDerive}

    implicit lazy val weirdThingSwagger: SwaggerTypeable[WeirdThing] =
      SwaggerTypeable.make(SwaggerPrimitive.boolean).as[WeirdThing]


    implicit lazy val innerShitSwagger: SwaggerTypeable[InnerShit] = magnoliaDerive
    implicit lazy val outerShitSwagger: SwaggerTypeable[OuterShit] = magnoliaDerive
    lazy val topShitSwagger: SwaggerTypeable[TopShit] = magnoliaDerive

    println(topShitSwagger.typ.collectTypes.foreach { case (name, t) => println(s"$name: ${t.asJson.pretty(printer)}") })
  }
}
