package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.Decompose.{Cons, Last, NotFound}
import ru.tinkoff.tschema.{Composite, Decompose, ResponseStatus}
import shapeless.Lazy
import cats.syntax.option._
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import ru.tinkoff.tschema.swagger.SwaggerContent.Content
import cats.syntax.foldable._

import scala.annotation.implicitNotFound

@implicitNotFound("SwaggerContent for ${T} is not found, try supply Swagger[T], e.g. using Swagger.derive")
final case class SwaggerContent[T](content: Content) {
  def collectTypes: Map[String, DescribedType] =
    content.foldLeft[Map[String, DescribedType]](Map()) { case (m, (_, ot)) => ot.foldLeft(m)(_ ++ _.collectTypes) }
}

/** can serve as Derivation target for sealed traits */
object SwaggerContent extends Derivation[SwaggerContent] {
  type Typeclass[A] = SwaggerContent[A]

  type Content = List[(Int, Option[SwaggerType])]

  def by = BuilderBy(Nil)

  final case class BuilderBy(types: Content) extends AnyVal {
    def apply[T](implicit t: SwaggerTypeable[T], s: ResponseStatus[T] = ResponseStatus.default[T]) =
      BuilderBy((s.status -> t.typ.some) :: types)
    def of[T]: SwaggerContent[T]                                                                   = SwaggerContent[T](types)
  }

  final implicit def bySingleTypeable[T](implicit
      t: SwaggerTypeable[T],
      s: ResponseStatus[T] = ResponseStatus.default[T]
  ): SwaggerContent[T] =
    SwaggerContent(List(s.status -> t.typ.some))

  final implicit val notFoundContent: SwaggerContent[NotFound.type] = SwaggerContent(List(404 -> None))
  final implicit val noneContent: SwaggerContent[None.type] = SwaggerContent(List(404 -> None))

  def combine[T](cc: CaseClass[Typeclass, T])(implicit no: SwaggerContentNotCombineable) = no.absurd

  def dispatch[T](st: SealedTrait[Typeclass, T]): SwaggerContent[T] =
    SwaggerContent(st.subtypes.toList.foldMap(_.typeclass.content))

  def instance[T]: SwaggerContent[T] = macro Magnolia.gen[T]
}

@implicitNotFound("SwagerContent could be derived only for sealed traits")
final abstract class SwaggerContentNotCombineable {
  def absurd: Nothing
}
