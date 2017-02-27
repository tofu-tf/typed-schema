package ru.tinkoff.tschema.swagger

import java.util.{Date, ResourceBundle}

import MkSwagger._
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.util.ByteString
import ru.tinkoff.tschema.named.Name
import ru.tinkoff.tschema.typeDSL._
import shapeless.Witness
import cats.{Monoid, MonoidK}

import scala.language.higherKinds

sealed trait MkSwagger[T] {
  self ⇒

  def paths: PathSeq

  def types: TypePool

  def as[U]: MkSwagger[U] = self.asInstanceOf[MkSwagger[U]]

  def map(f: PathSpec ⇒ PathSpec) = new MkSwagger[T] {
    val paths = self.paths.map(f)
    val types = self.types
  }

  def ++(other: MkSwagger[_]) = new MkSwagger[T] {
    val paths = self.paths ++ other.paths
    val types = self.types ++ other.types
  }

  def make(info: SwaggerInfo) =
    Swagger(
      info = info,
      paths = paths.groupBy(_.path).map { case (parts, specs) ⇒ parts.mkString("/", "/", "") → specs.map { case PathSpec(_, method, op) ⇒ method -> op }.toMap },
      definitions = types
    )

  def addResponse[U](code: StatusCode, description: Option[SwaggerDescription] = None)(implicit typeable: SwaggerTypeable[U]) =
    new MkSwagger[U] {
      val paths: PathSeq = self.paths.map(_.modOp { op ⇒
        val codes = op.responses.codes
        op.copy(responses = op.responses.copy(codes = codes ++ Map(
          code → SwaggerResponse(schema = typeable.swaggerType, description = description)
        )))
      })
      val types: TypePool = self.types ++ typeable.swaggerType.collectTypes
    }
}

object MkSwagger {
  def empty[T]: MkSwagger[T] = new MkSwagger[T] {
    override def paths = Vector.empty
    override def types = Map.empty
  }

  def apply[T](implicit derived: MkSwagger[T]): MkSwagger[T] = derived

  case class PathSpec(path: Vector[String], method: Swagger.Method, op: SwaggerOp) {
    def modPath(f: Vector[String] ⇒ Vector[String]) = copy(path = f(path))
    def modOp(f: SwaggerOp ⇒ SwaggerOp) = copy(op = f(op))
  }

  type PathSeq = Vector[PathSpec]

  type TypePool = Map[String, SwaggerType]

  def single[T](op: SwaggerOp, method: Swagger.Method, typeList: Map[String, SwaggerType]) = new MkSwagger[T] {
    val paths = Vector(PathSpec(Vector.empty, method, op))
    val types = typeList
  }

  private def derivedMethod[T, meth[_]](method: Swagger.Method)(implicit typ: SwaggerTypeable[T]) =
    single[meth[T]](
      method = method,
      op = SwaggerOp(
        responses = SwaggerResponses(codes = Map(
          StatusCodes.OK → SwaggerResponse(
            schema = typ.swaggerType
          )))),
      typeList = typ.swaggerType.collectTypes)

  implicit def deriveGet[T: SwaggerTypeable] = derivedMethod[T, Get](Swagger.Method.get)
  implicit def derivePost[T: SwaggerTypeable] = derivedMethod[T, Post](Swagger.Method.post)
  implicit def deriveHead[T: SwaggerTypeable] = derivedMethod[T, Head](Swagger.Method.head)
  implicit def derivePut[T: SwaggerTypeable] = derivedMethod[T, Put](Swagger.Method.put)
  implicit def deriveDelete[T: SwaggerTypeable] = derivedMethod[T, Delete](Swagger.Method.delete)
  implicit def deriveOptions[T: SwaggerTypeable] = derivedMethod[T, Options](Swagger.Method.options)

  implicit def deriveJoin[left, right]
  (implicit left: MkSwagger[left], right: MkSwagger[right]) = (left ++ right).as[left <|> right]

  implicit def deriveCons[start, end]
  (implicit start: SwaggerMapper[start], end: MkSwagger[end]): MkSwagger[start :> end] = start(end).as[start :> end]

  implicit val monoidKInstance = new MonoidK[MkSwagger] {
    def empty[A]: MkSwagger[A] = MkSwagger.empty[A]
    def combineK[A](x: MkSwagger[A], y: MkSwagger[A]): MkSwagger[A] = x ++ y
  }
  implicit def monoidInstance[A] = monoidKInstance.algebra[A]
}

case class AsSwaggerParam[T](value: SwaggerValue, required: Boolean = true)

object AsSwaggerParam {
  implicit lazy val booleanParam = AsSwaggerParam[Boolean](SwaggerBooleanValue())

  implicit lazy val intParam = AsSwaggerParam[Int](SwaggerIntValue(format = Some(SwaggerFormat.int32)))
  implicit lazy val longParam = AsSwaggerParam[Long](SwaggerIntValue(format = Some(SwaggerFormat.int64)))
  implicit lazy val bigIntParam = AsSwaggerParam[BigInt](SwaggerIntValue())

  implicit lazy val floatParam = AsSwaggerParam[Float](SwaggerNumberValue(format = Some(SwaggerFormat.float)))
  implicit lazy val doubleParam = AsSwaggerParam[Double](SwaggerNumberValue(format = Some(SwaggerFormat.double)))
  implicit lazy val bigDecimalParam = AsSwaggerParam[BigDecimal](SwaggerNumberValue())

  implicit lazy val stringParam = AsSwaggerParam[String](SwaggerStringValue())
  implicit lazy val byteParam = AsSwaggerParam[Byte](SwaggerStringValue(format = Some(SwaggerFormat.byte)))
  implicit lazy val byteStringParam = AsSwaggerParam[ByteString](SwaggerStringValue(format = Some(SwaggerFormat.binary)))
  implicit lazy val byteArrayParam = AsSwaggerParam[Array[Byte]](SwaggerStringValue(format = Some(SwaggerFormat.binary)))
  implicit lazy val utilDateParam = AsSwaggerParam[Date](SwaggerStringValue(format = Some(SwaggerFormat.dateTime)))

  implicit def optionParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[Option[T]](param.value, required = false)
  implicit def vectorParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[Vector[T]](SwaggerArrayValue(param.value), param.required)
  implicit def listParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[List[T]](SwaggerArrayValue(param.value), param.required)
  implicit def streamParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[Stream[T]](SwaggerArrayValue(param.value), param.required)
}

trait SwaggerMapper[T] {
  self ⇒
  def mapSpec(spec: PathSpec): PathSpec
  def types: Map[String, SwaggerType]

  def andThen[B](other: SwaggerMapper[B]) = new SwaggerMapper[B] {
    def mapSpec(spec: PathSpec): PathSpec = self.mapSpec(other.mapSpec(spec))
    val types = self.types ++ other.types
  }

  def map(f: PathSpec ⇒ PathSpec) = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = f(self.mapSpec(spec))
    def types: Map[String, SwaggerType] = self.types
  }

  def ++(that: TraversableOnce[(String, SwaggerType)]) = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = self.mapSpec(spec)
    def types: Map[String, SwaggerType] = self.types ++ that
  }

  def apply(mkSwagger: MkSwagger[_]) = new MkSwagger[T] {
    def paths: PathSeq = mkSwagger.paths.map(mapSpec)
    def types: TypePool = mkSwagger.types ++ self.types
  }

  def as[U] = self.asInstanceOf[SwaggerMapper[U]]
}

object SwaggerMapper {
  def empty[T] = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = spec
    def types: Map[String, SwaggerType] = Map.empty
  }

  abstract class FromFunc[T] extends SwaggerMapper[T] {
    def types: Map[String, SwaggerType] = Map.empty
  }

  abstract class FromTypes[T] extends SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = spec
  }

  import SwaggerParam.{In, NonBodyIn}

  def fromFunc[T](f: PathSpec ⇒ PathSpec) = new FromFunc[T] {
    def mapSpec(spec: PathSpec): PathSpec = f(spec)
  }

  def fromTypes[T](tps: Map[String, SwaggerType]) = new FromTypes[T] {
    def types: Map[String, SwaggerType] = tps
  }

  private def derivedParam[name, T, param[_, _]]
  (in: NonBodyIn)
  (implicit name: Name[name], param: AsSwaggerParam[T]) =
    fromFunc[param[name, T]](_.modOp(_.addParam(SwaggerParam(
      base = SwaggerParamBase(name.string, required = param.required),
      specific = SwaggerParamGeneral(
        in = in,
        value = param.value)))))

  implicit def derivePath[path](implicit name: Name[path]) =
    fromFunc[path](_.modPath(name.string +: _))

  implicit def derivePathPrefix[path](implicit name: Name[path]) =
    fromFunc[Prefix[path]](_.modPath(name.string +: _))

  implicit def derivePathWitness[path](implicit name: Name[path]) =
    fromFunc[Witness.Aux[path]](_.modPath(name.string +: _))

  implicit def deriveQueryParam[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, QueryParam](In.query)

  implicit def deriveQueryFlag[name: Name]: SwaggerMapper[QueryFlag[name]] =
    derivedParam[name, Option[Boolean], λ[(n, x) => QueryFlag[n]]](In.query)

  implicit def deriveHeader[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, Header](In.header)

  implicit def deriveFormField[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, FormField](In.formData)

  implicit def deriveCookie[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, Cookie](In.cookie)

  implicit def derivePathParam[name, T: AsSwaggerParam](implicit name: Name[name]) =
    derivedParam[name, T, Capture](In.path) map (_.modPath(s"{$name}" +: _))

  implicit def deriveReqBody[T](implicit typeable: SwaggerTypeable[T]): SwaggerMapper[ReqBody[T]] =
    new SwaggerMapper[ReqBody[T]] {
      def mapSpec(spec: PathSpec): PathSpec = spec.modOp(_.addParam(SwaggerParam(
        base = SwaggerParamBase("body", required = true),
        specific = SwaggerParamBody(typeable.swaggerType))))
      val types: Map[String, SwaggerType] = typeable.swaggerType.collectTypes
    }

  implicit def deriveCons[start, end]
  (implicit start: SwaggerMapper[start], end: SwaggerMapper[end]): SwaggerMapper[start :> end] = (start andThen end).as[start :> end]

  private def deriveDescription[T](descr: SwaggerDescription): SwaggerMapper[T] =
    new FromFunc[T] {
      def mapSpec(spec: PathSpec): PathSpec = spec.modOp(_.copy(description = Some(descr)))
    }

  implicit def deriveStaticDescr[name](implicit name: Name[name]) =
    deriveDescription[Description.Static[name]](StaticDescription(name.string))

  implicit def deriveResourceDescr[name](implicit name: Name[name]) =
    deriveDescription[Description.Resource[name]](ResourceDescription(name.string))

  implicit def deriveI18nDescr[name]
  (implicit name: Name[name], bundle: ResourceBundle): SwaggerMapper[Description.I18n[name]] =
    deriveDescription[Description.I18n[name]](I18nDescription(name.string))

  implicit def deriveTag[name](implicit name: Name[name]): SwaggerMapper[Tag[name]] =
    new FromFunc[Tag[name]] {
      def mapSpec(spec: PathSpec): PathSpec = spec.modOp(_.addTag(name.string))
    }

  implicit def deriveKey[name] = empty[Key[name]]

  implicit val monoidKInstance = new MonoidK[SwaggerMapper] {
    def empty[A]: SwaggerMapper[A] = SwaggerMapper.empty
    def combineK[A](x: SwaggerMapper[A], y: SwaggerMapper[A]): SwaggerMapper[A] = x andThen y
  }

  implicit def monoidInstance[A] = monoidKInstance.algebra[A]
}




