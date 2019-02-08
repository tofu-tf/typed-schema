package ru.tinkoff.tschema.swagger

sealed trait AsOpenApiParam[T] {
  def types: Map[String, DescribedType]
}

object AsOpenApiParam extends AsOpenParamInstances[AsOpenApiParam]

trait OpenApiSingleInfo {
  def typ: SwaggerType
  def required: Boolean
  def types = typ.collectTypes
}
final case class OpenApiParamField(name: String, typ: SwaggerType, required: Boolean) extends OpenApiSingleInfo

final case class AsMultiOpenApiParam[T](fields: Vector[OpenApiParamField]) {
  def types = fields.foldLeft(Map.empty[String, DescribedType])(_ ++ _.types)
}
final case class AsSingleOpenApiParam[T](typ: SwaggerType, required: Boolean = true)
    extends AsOpenApiParam[T] with OpenApiSingleInfo {}

object AsSingleOpenApiParam extends AsOpenParamInstances[AsSingleOpenApiParam]

trait AsOpenParamInstances[TC[x] >: AsSingleOpenApiParam[x]] {
  final implicit def requiredParam[T](implicit typ: SwaggerTypeable[T]): TC[T] =
    AsSingleOpenApiParam[T](typ = typ.typ, required = true)
  final implicit def optionalParam[T](implicit typ: SwaggerTypeable[T]): TC[Option[T]] =
    AsSingleOpenApiParam[Option[T]](typ = typ.typ, required = false)
}
