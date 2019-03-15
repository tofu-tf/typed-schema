package ru.tinkoff.tschema.swagger

import scala.annotation.implicitNotFound

@implicitNotFound("SwaggerContent for ${T} is not found, try supply SwaggerTypeable for T, e.g. using MagnoliaSwagger.derive")
final case class SwaggerContent[T](types: List[SwaggerType]) {
  def collectTypes: Map[String, DescribedType] =
    types.foldLeft[Map[String, DescribedType]](Map())(_ ++ _.collectTypes)
}

object SwaggerContent {
  def by = BuilderBy(Nil)

  final case class BuilderBy(types: List[SwaggerType]) extends AnyVal {
    def apply[T](implicit t: SwaggerTypeable[T]) = BuilderBy(t.typ :: types)
    def of[T]: SwaggerContent[T]                 = SwaggerContent[T](types)
  }

  final implicit def bySingleTypeable[T](implicit t: SwaggerTypeable[T]): SwaggerContent[T] =
    SwaggerContent(List(t.typ))
}
