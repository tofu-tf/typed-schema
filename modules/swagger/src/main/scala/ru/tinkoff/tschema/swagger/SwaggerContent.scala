package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.Decompose.{Cons, Last, NotFound}
import ru.tinkoff.tschema.{Composite, Decompose, ResponseStatus}
import shapeless.Lazy
import cats.syntax.option._
import ru.tinkoff.tschema.swagger.SwaggerContent.Content

import scala.annotation.implicitNotFound

@implicitNotFound("SwaggerContent for ${T} is not found, try supply SwaggerTypeable for T, e.g. using MagnoliaSwagger.derive")
final case class SwaggerContent[T](content: Content) {
  def collectTypes: Map[String, DescribedType] =
    content.foldLeft[Map[String, DescribedType]](Map()) { case (m, (_, ot)) => ot.foldLeft(m)(_ ++ _.collectTypes) }
}

object SwaggerContent extends CompositeInstances {
  type Content = List[(Int, Option[SwaggerType])]

  def by = BuilderBy(Nil)

  final case class BuilderBy(types: Content) extends AnyVal {
    def apply[T](implicit t: SwaggerTypeable[T], s: ResponseStatus[T] = ResponseStatus.default[T]) =
      BuilderBy((s.status -> t.typ.some) :: types)
    def of[T]: SwaggerContent[T] = SwaggerContent[T](types)
  }

  final implicit def bySingleTypeable[T](implicit t: SwaggerTypeable[T],
                                         s: ResponseStatus[T] = ResponseStatus.default[T]): SwaggerContent[T] =
    SwaggerContent(List(s.status -> t.typ.some))

  final implicit val notFoundContent: SwaggerContent[NotFound.type] = SwaggerContent(List(404 -> None))
}

class CompositeInstances {
  final implicit def decomposeInstance[A, D](implicit d: Decompose.Aux[A, D],
                                             content: Lazy[CompositeContent[A, D]]): SwaggerContent[Composite[A]] =
    SwaggerContent(content.value.content)
}

final case class CompositeContent[A, D](content: Content)

object CompositeContent {
  implicit def nilContent[A]: CompositeContent[A, Last[A]] = CompositeContent(Nil)

  implicit def consContent[A, H, D <: Decompose[A]](
      implicit head: SwaggerContent[H],
      tail: CompositeContent[A, D]
  ): CompositeContent[A, Cons[A, H, D]] = CompositeContent(head.content ::: tail.content)
}
