package tschema.swagger.testDomain

trait WeirdThing



sealed trait LotOfVariants

case object AnotherLol                                         extends LotOfVariants
final case class OuterStuff(foo: Int, inner: List[InnerStuff]) extends LotOfVariants
final case class TopStuff(vars: LotOfVariants)                 extends LotOfVariants

final case class InnerStuff(shouldFind: String, shouldNotFind: WeirdThing, outer: Option[OuterStuff])
