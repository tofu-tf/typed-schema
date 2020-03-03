package ru.tinkoff.tschema.swagger.testDomain
import derevo.derive
import ru.tinkoff.tschema.swagger.AsOpenApiParam
import tschema.swagger.Swagger

trait WeirdThing



sealed trait LotOfVariants

case object AnotherLol                                         extends LotOfVariants
final case class OuterStuff(foo: Int, inner: List[InnerStuff]) extends LotOfVariants
final case class TopStuff(vars: LotOfVariants)                 extends LotOfVariants

final case class InnerStuff(shouldFind: String, shouldNotFind: WeirdThing, outer: Option[OuterStuff])


@derive(Swagger, AsOpenApiParam)
case class Person(
  name: String,
  lastName: Option[String],
  child: Option[Person]
)
