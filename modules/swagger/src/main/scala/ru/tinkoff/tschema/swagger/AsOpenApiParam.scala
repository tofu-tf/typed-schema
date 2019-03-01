package ru.tinkoff.tschema.swagger
import cats.data.NonEmptyList
import magnolia.{CaseClass, Magnolia, SealedTrait}

sealed trait AsOpenApiParam[T] {
  def types: Map[String, DescribedType]
  def optional: AsOpenApiParam[Option[T]]
}

object AsOpenApiParam extends AsOpenParamInstances[AsOpenApiParam] {
  type Typeclass[x] = AsOpenApiParam[x]

  def apply[T](param: AsOpenApiParam[T]): AsOpenApiParam[T] = param

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
    AsMultiOpenApiParam[T](
      NonEmptyList
        .fromListUnsafe(ctx.parameters.toList)
        .flatMap { param =>
          param.typeclass match {
            case AsSingleOpenApiParam(t, r) => NonEmptyList.of(OpenApiParamField(param.label, t, r))
            case AsMultiOpenApiParam(ps)    => ps
          }
        })

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  def generate[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait OpenApiParamInfo {
  def typ: SwaggerType
  def required: Boolean
  def types = typ.collectTypes
}
final case class OpenApiParamField(name: String, typ: SwaggerType, required: Boolean) extends OpenApiParamInfo

final case class AsMultiOpenApiParam[T](fields: NonEmptyList[OpenApiParamField]) extends AsOpenApiParam[T] {
  def parts: NonEmptyList[String]              = fields.map(_.name)
  def types                                    = fields.foldLeft(Map.empty[String, DescribedType])(_ ++ _.types)
  def optional: AsMultiOpenApiParam[Option[T]] = AsMultiOpenApiParam(fields.map(_.copy(required = false)))
}
final case class AsSingleOpenApiParam[T](typ: SwaggerType, required: Boolean = true)
    extends AsOpenApiParam[T] with OpenApiParamInfo {
  def optional: AsOpenApiParam[Option[T]] = AsSingleOpenApiParam(typ, required = false)
}

object AsSingleOpenApiParam extends AsOpenParamInstances[AsSingleOpenApiParam]

trait AsOpenParamInstances[TC[x] >: AsSingleOpenApiParam[x]]{
  final implicit def requiredParam[T](implicit typ: SwaggerTypeable[T]): TC[T] =
    AsSingleOpenApiParam[T](typ = typ.typ, required = true)
  final implicit def optParam[T](implicit param: AsOpenApiParam[T]): AsOpenApiParam[Option[T]] = param.optional
}

