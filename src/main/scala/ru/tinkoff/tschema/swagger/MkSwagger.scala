package ru.tinkoff.tschema.swagger

import java.util.{Date, ResourceBundle}

import MkSwagger._
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.util.ByteString
import ru.tinkoff.tschema.named.Name
import ru.tinkoff.tschema.typeDSL._
import shapeless.Witness

import scala.language.higherKinds

sealed trait MkSwagger {
  def paths: PathSeq

  def types: TypePool

  def map(f: PathSpec ⇒ PathSpec) = Mapper(func = f)(this)

  def ++(other: MkSwagger) = (this, other) match {
    case (Cons(left), Cons(right)) ⇒ Cons(left ++ right)
    case (Cons(makers), _) ⇒ Cons(makers :+ other)
    case (_, Cons(makers)) ⇒ Cons(this +: makers)
    case (_, _) ⇒ Cons(Vector(this, other))
  }

  def make(info: SwaggerInfo) =
    Swagger(
      info = info,
      paths = paths.groupBy(_.path).map { case (parts, specs) ⇒ parts.mkString("/", "/", "") → specs.map { case PathSpec(_, method, op) ⇒ method -> op }.toMap },
      definitions = types
    )

  def addResponse[T](code: StatusCode, description: Option[SwaggerDescription] = None)(implicit typeable: SwaggerTypeable[T]) =
    Mapper(_.modOp { op ⇒
      val codes = op.responses.codes
      op.copy(responses = op.responses.copy(codes = codes ++ Map(
        code → SwaggerResponse(schema = typeable.swaggerType, description = description)
      )))
    }, typeable.swaggerType.collectTypes)(this)
}

object MkSwagger {
  val empty: MkSwagger = new MkSwagger {
    override def paths = Vector.empty
    override def types = Map.empty
  }

  def apply[T](implicit derived: DerivedMkSwagger[T]): MkSwagger = derived.mkSwagger

  case class PathSpec(path: Vector[String], method: Swagger.Method, op: SwaggerOp) {
    def modPath(f: Vector[String] ⇒ Vector[String]) = copy(path = f(path))
    def modOp(f: SwaggerOp ⇒ SwaggerOp) = copy(op = f(op))
  }

  case class Mapper(func: PathSpec ⇒ PathSpec = identity, types: Map[String, SwaggerType] = Map.empty) {
    def >>(other: Mapper) = Mapper(other.func andThen func, types ++ other.types)
    def andThen(f: PathSpec ⇒ PathSpec) = copy(func = func andThen f)
    def ++(that: TraversableOnce[(String, SwaggerType)]) = copy(types = types ++ that)
    def apply(mkSwagger: MkSwagger) = mkSwagger match {
      case Mapping(sub, that) ⇒ Mapping(sub, this >> that)
      case _ ⇒ Mapping(mkSwagger, this)
    }
  }

  type PathSeq = Vector[PathSpec]

  type TypePool = Map[String, SwaggerType]

  case class Single(op: SwaggerOp, method: Swagger.Method, types: Map[String, SwaggerType]) extends MkSwagger {
    def paths = Vector(PathSpec(Vector.empty, method, op))
  }

  case class Cons(makers: Vector[MkSwagger]) extends MkSwagger {
    override def paths = makers.flatMap(_.paths)
    override def types = makers.view.map(_.types).reduce(_ ++ _)
  }

  case class Mapping(sub: MkSwagger, mapper: Mapper) extends MkSwagger {
    override def paths = sub.paths.map(mapper.func)
    override def types = mapper.types ++ sub.types
  }
}

class DerivedMkSwagger[T](val mkSwagger: MkSwagger) extends AnyVal

trait LowPriorityMkSwagger {
  implicit def deriveCons[start, end]
  (implicit start: DerivedMkSwaggerPrefix[start], end: DerivedMkSwagger[end]) =
    new DerivedMkSwagger[start :> end](start.mapper(end.mkSwagger))
}

object DerivedMkSwagger extends LowPriorityMkSwagger {
  def apply[T](implicit derived: DerivedMkSwagger[T]) = derived.mkSwagger

  private def derivedMethod[T, meth[_]](method: Swagger.Method)(implicit typ: SwaggerTypeable[T]) =
    new DerivedMkSwagger[meth[T]](Single(
      method = method,
      op = SwaggerOp(
        responses = SwaggerResponses(codes = Map(
          StatusCodes.OK → SwaggerResponse(
            schema = typ.swaggerType
          )))),
      types = typ.swaggerType.collectTypes))

  implicit def deriveGet[T: SwaggerTypeable] = derivedMethod[T, Get](Swagger.Method.get)
  implicit def derivePost[T: SwaggerTypeable] = derivedMethod[T, Post](Swagger.Method.post)

  implicit def deriveJoin[left, right]
  (implicit left: DerivedMkSwagger[left], right: DerivedMkSwagger[right]) =
    new DerivedMkSwagger[left <|> right](left.mkSwagger ++ right.mkSwagger)
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

case class DerivedMkSwaggerPrefix[T](mapper: Mapper) extends AnyVal {
  def andThen(f: PathSpec ⇒ PathSpec) = DerivedMkSwaggerPrefix[T](mapper.andThen(f))
  def addTypes(types: Map[String, SwaggerType]) = DerivedMkSwaggerPrefix[T](mapper ++ types)
}

object DerivedMkSwaggerPrefix {

  import SwaggerParam.{In, NonBodyIn}

  def fromFunc[T](f: PathSpec ⇒ PathSpec) = DerivedMkSwaggerPrefix[T](Mapper(func = f))
  def fromTypes[T](types: Map[String, SwaggerType]) = DerivedMkSwaggerPrefix[T](Mapper(types = types))

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

  implicit def deriveQueryFlag[name: Name]: DerivedMkSwaggerPrefix[QueryFlag[name]] =
    derivedParam[name, Option[Boolean], λ[(n, x) => QueryFlag[n]]](In.query)

  implicit def deriveHeader[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, Header](In.header)

  implicit def deriveFormField[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, FormField](In.formData)

  implicit def deriveCookie[name: Name, T: AsSwaggerParam] =
    derivedParam[name, T, Cookie](In.cookie)

  implicit def derivePathParam[name, T: AsSwaggerParam](implicit name: Name[name]) =
    derivedParam[name, T, Capture](In.path) andThen (_.modPath(s"{$name}" +: _))

  implicit def deriveReqBody[T](implicit typeable: SwaggerTypeable[T]) =
    fromFunc[ReqBody[T]](_.modOp(_.addParam(SwaggerParam(
      base = SwaggerParamBase("body", required = true),
      specific = SwaggerParamBody(typeable.swaggerType)))))
      .addTypes(typeable.swaggerType.collectTypes)

  implicit def deriveCons[start, end]
  (implicit start: DerivedMkSwaggerPrefix[start], end: DerivedMkSwaggerPrefix[end]) =
    new DerivedMkSwaggerPrefix[start :> end](start.mapper >> end.mapper)

  private def deriveDescription[T](descr: SwaggerDescription) =
    new DerivedMkSwaggerPrefix[T](Mapper(func = _.modOp(_.copy(description = Some(descr)))))

  implicit def deriveStaticDescr[name](implicit name: Name[name]) =
    deriveDescription[Description.Static[name]](StaticDescription(name.string))

  implicit def deriveResourceDescr[name](implicit name: Name[name]) =
    deriveDescription[Description.Resource[name]](ResourceDescription(name.string))

  implicit def deriveI18nDescr[name]
  (implicit name: Name[name], bundle: ResourceBundle): DerivedMkSwaggerPrefix[Description.I18n[name]] =
    deriveDescription[Description.I18n[name]](I18nDescription(name.string))

  implicit def deriveTag[name](implicit name: Name[name]) =
    new DerivedMkSwaggerPrefix[Tag[name]](Mapper(func = _.modOp(_.addTag(name.string))))
}




