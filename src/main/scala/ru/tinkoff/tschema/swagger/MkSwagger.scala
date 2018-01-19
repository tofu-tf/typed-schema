package ru.tinkoff.tschema.swagger

import java.util.{Date, ResourceBundle, UUID}

import akka.http.scaladsl.model.{MediaTypes, StatusCode, StatusCodes}
import akka.util.ByteString
import cats.MonoidK
import cats.arrow.FunctionK
import cats.syntax.option._
import monocle.macros.Lenses
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.swagger.MkSwagger._
import ru.tinkoff.tschema.typeDSL._
import shapeless.Witness

import monocle.function.Each.each
import monocle.std.option.some

import scala.collection.immutable.TreeMap
import scala.language.higherKinds


sealed trait MkSwagger[T] {
  self =>

  def paths: PathSeq

  def types: TypePool

  def as[U]: MkSwagger[U] = self.asInstanceOf[MkSwagger[U]]

  def map(f: PathSpec => PathSpec) = new MkSwagger[T] {
    val paths = self.paths.map(f)
    val types = self.types
  }

  def ++(other: MkSwagger[_]) = new MkSwagger[T] {
    val paths = self.paths ++ other.paths
    val types = self.types ++ other.types
  }

  def make(info: OpenApiInfo) = {
    val openApiPaths =
      paths.groupBy(_.path)
        .map {
          case (parts, specs) => parts.mkString("/", "/", "") ->
            specs.collect { case PathSpec(_, Some(method), op, _) => method -> op }.toMap
        }
    OpenApi(
      info = info,
      paths = TreeMap(openApiPaths.toSeq: _*),
      components = OpenApiComponents(schemas = types)
    )
  }

  def addResponse[U](code: StatusCode, description: Option[SwaggerDescription] = None)(implicit typeable: SwaggerTypeable[U]) =
    new MkSwagger[U] {
      val paths: PathSeq = self.paths.map(
        (PathSpec.op ^|-> OpenApiOp.responses ^|-> OpenApiResponses.codes).modify(_ + (
          code -> OpenApiResponse.make(description = description,
            swaggerType = typeable.typ)))
      )

      val types: TypePool = self.types ++ typeable.typ.collectTypes
    }

  def describe(descriptions: String => Option[PathDescription]): MkSwagger[T] =
    map {
      case spec@PathSpec(_, _, _, Some(key)) =>
        descriptions(key).fold(spec) { case PathDescription(description, params) =>
          PathSpec.op.modify(
            OpenApiOp.description.modify(description orElse _) andThen
              OpenApiOp.parameters.composeTraversal(each).modify(param => OpenApiParam.description.modify(params(param.name) orElse _)(param)) andThen
            OpenApiOp.requestBody.composePrism(some).composeLens(OpenApiRequestBody.description).modify(params("body") orElse _)
          )(spec)
        }
      case spec                              => spec
    }
}

object MkSwagger {
  object macroInterface {}

  def empty[T]: MkSwagger[T] = new MkSwagger[T] {
    override def paths = Vector.empty
    override def types = TreeMap.empty
  }

  def apply[T](implicit derived: MkSwagger[T]): MkSwagger[T] = derived

  @Lenses
  case class PathSpec(path: Vector[String], method: Option[OpenApi.Method], op: OpenApiOp, key: Option[String] = None) {
    def modPath(f: Vector[String] => Vector[String]) = PathSpec.path.modify(f)(this)
  }

  type PathSeq = Vector[PathSpec]

  type TypePool = TreeMap[String, DescribedType]

  def single[T](op: OpenApiOp, typeList: TreeMap[String, DescribedType]) = new MkSwagger[T] {
    val paths = Vector(PathSpec(Vector.empty, None, op))
    val types = typeList
  }

  implicit def derivedComplete[T](implicit typ: SwaggerTypeable[T]) =
    single[Complete[T]](
      op = OpenApiOp(
        responses = OpenApiResponses(codes = Map(
          StatusCodes.OK -> OpenApiResponse.make(
            swaggerType = typ.typ
          )))),
      typeList = TreeMap(typ.typ.collectTypes.toSeq: _*))

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

final case class AsOpenApiParam[T](typ: SwaggerType, required: Boolean = true) {
  def types = typ.collectTypes
}

object AsOpenApiParam {
  implicit def requiredParam[T](implicit typ: SwaggerTypeable[T]): AsOpenApiParam[T] = AsOpenApiParam[T](typ = typ.typ, required = true)
  implicit def optionalParam[T](implicit typ: SwaggerTypeable[T]): AsOpenApiParam[Option[T]] = AsOpenApiParam[Option[T]](typ = typ.typ, required = false)
}


case class AsSwaggerParam[T](value: SwaggerValue, required: Boolean = true)

object AsSwaggerParam {
  implicit lazy val booleanParam = AsSwaggerParam[Boolean](SwaggerBooleanValue())

  implicit lazy val intParam = AsSwaggerParam[Int](SwaggerIntValue(format = OpenApiFormat.int32.some))
  implicit lazy val longParam = AsSwaggerParam[Long](SwaggerIntValue(format = OpenApiFormat.int64.some))
  implicit lazy val bigIntParam = AsSwaggerParam[BigInt](SwaggerIntValue())

  implicit lazy val floatParam = AsSwaggerParam[Float](SwaggerNumberValue(format = OpenApiFormat.float.some))
  implicit lazy val doubleParam = AsSwaggerParam[Double](SwaggerNumberValue(format = OpenApiFormat.double.some))
  implicit lazy val bigDecimalParam = AsSwaggerParam[BigDecimal](SwaggerNumberValue())

  implicit lazy val stringParam = AsSwaggerParam[String](SwaggerStringValue())
  implicit lazy val byteParam = AsSwaggerParam[Byte](SwaggerStringValue(format = OpenApiFormat.byte.some))

  implicit lazy val byteStringParam = AsSwaggerParam[ByteString](SwaggerStringValue(format = OpenApiFormat.binary.some))
  implicit lazy val byteArrayParam = AsSwaggerParam[Array[Byte]](SwaggerStringValue(format = OpenApiFormat.binary.some))
  implicit lazy val utilDateParam = AsSwaggerParam[Date](SwaggerStringValue(format = OpenApiFormat.dateTime.some))
  implicit lazy val uuidParam = AsSwaggerParam[UUID](SwaggerStringValue.uuid)

  implicit def optionParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[Option[T]](param.value, required = false)
  implicit def vectorParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[Vector[T]](SwaggerArrayValue(param.value), param.required)
  implicit def listParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[List[T]](SwaggerArrayValue(param.value), param.required)
  implicit def streamParam[T](implicit param: AsSwaggerParam[T]) = AsSwaggerParam[Stream[T]](SwaggerArrayValue(param.value), param.required)

  trait Enum[E <: enumeratum.EnumEntry] {
    self: enumeratum.Enum[E] =>
    implicit lazy val asSwaggerParam: AsSwaggerParam[E] =
      AsSwaggerParam[E](SwaggerStringValue(enum = self.values.map(_.entryName).toVector.some))
  }
}

trait SwaggerMapper[T] extends FunctionK[MkSwagger, MkSwagger] {
  self =>
  def mapSpec(spec: PathSpec): PathSpec
  def types: Map[String, DescribedType]

  def apply[A](mkSwagger: MkSwagger[A]): MkSwagger[A] = new MkSwagger[A] {
    def paths: PathSeq = mkSwagger.paths.map(mapSpec)
    def types: TypePool = mkSwagger.types ++ self.types
  }

  def andThen[B](other: SwaggerMapper[B]) = new SwaggerMapper[B] {
    def mapSpec(spec: PathSpec): PathSpec = self.mapSpec(other.mapSpec(spec))
    val types = self.types ++ other.types
  }

  def map(f: PathSpec => PathSpec) = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = f(self.mapSpec(spec))
    def types: Map[String, DescribedType] = self.types
  }

  def ++(that: TraversableOnce[(String, SwaggerType)]) = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = self.mapSpec(spec)
    def types: Map[String, DescribedType] = self.types ++ that.map { case (name, typ) => name -> DescribedType(typ) }
  }

  def as[U] = self.asInstanceOf[SwaggerMapper[U]]
}

object SwaggerMapper {
  def apply[T](implicit mapper: SwaggerMapper[T]): SwaggerMapper[T] = mapper

  def empty[T] = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = spec
    def types: Map[String, DescribedType] = TreeMap.empty
  }

  abstract class FromFunc[T] extends SwaggerMapper[T] {
    def types: Map[String, DescribedType] = TreeMap.empty
  }

  abstract class FromTypes[T] extends SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = spec
  }

  import OpenApiParam.In

  def fromFunc[T](f: PathSpec => PathSpec) = new FromFunc[T] {
    def mapSpec(spec: PathSpec): PathSpec = f(spec)
  }

  def fromTypes[T](tps: Map[String, DescribedType]) = new FromTypes[T] {
    def types: Map[String, DescribedType] = tps
  }

  private def derivedParam[name, T, param[_, _]]
  (in: In)
    (implicit name: Name[name], param: AsOpenApiParam[T]): SwaggerMapper[param[name, T]] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.parameters).modify(_ :+ OpenApiParam(
      name = name.string,
      in = in,
      required = param.required,
      schema = param.typ.some))) andThen
      fromTypes[param[name, T]](param.types)


  implicit def derivePath[path](implicit name: Name[path]) =
    fromFunc[path](_.modPath(name.string +: _))

  implicit def derivePathPrefix[path](implicit name: Name[path]) =
    fromFunc[Prefix[path]](_.modPath(name.string +: _))

  implicit def derivePathWitness[path](implicit name: Name[path]) =
    fromFunc[Witness.Aux[path]](_.modPath(name.string +: _))

  implicit def deriveQueryParam[name: Name, T: AsOpenApiParam] =
    derivedParam[name, T, QueryParam](In.query)

  implicit def deriveQueryFlag[name: Name]: SwaggerMapper[QueryFlag[name]] =
    derivedParam[name, Option[Boolean], λ[(n, x) => QueryFlag[n]]](In.query)

  implicit def deriveHeader[name: Name, T: AsOpenApiParam] =
    derivedParam[name, T, Header](In.header)

  implicit def deriveFormField[name: Name, T: AsOpenApiParam] =
    derivedParam[name, T, FormField](In.formData)

  implicit def deriveCookie[name: Name, T: AsOpenApiParam] =
    derivedParam[name, T, Cookie](In.cookie)

  implicit def derivePathParam[name, T: AsOpenApiParam](implicit name: Name[name]) =
    derivedParam[name, T, Capture](In.path).map(PathSpec.path.modify(s"{$name}" +: _))

  implicit def deriveReqBody[T](implicit typeable: SwaggerTypeable[T]): SwaggerMapper[ReqBody[T]] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.requestBody).set(OpenApiRequestBody.fromType(typeable.typ).some)) andThen fromTypes[ReqBody[T]](typeable.typ.collectTypes)
  //
  //  implicit val deriveXML: SwaggerMapper[XML] = fromFunc[XML](
  //    (PathSpec.op ^|-> OpenApiOp.responses ^|-> OpenApiResponses.codes composeSetter setters.map ^|-> OpenApiResponse.content).modify(
  //      m => m.get(None).fold(m)(mt => m - None + (Some(MediaTypes.`application/xml`) -> mt))))

  implicit def deriveMethod[method](implicit methodDeclare: MethodDeclare[method]): SwaggerMapper[method] =
    fromFunc[method](PathSpec.method.modify {
      case None         => methodDeclare.method.some
      case meth@Some(_) => meth
    })

  implicit def deriveCons[start, end]
  (implicit start: SwaggerMapper[start], end: SwaggerMapper[end]): SwaggerMapper[start :> end] = (start andThen end).as[start :> end]

  private def deriveDesr[T](descr: SwaggerDescription): SwaggerMapper[T] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.description).set(descr.some))

  implicit def deriveTag[name](implicit name: Name[name]): SwaggerMapper[Tag[name]] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.tags).modify(_ :+ name.string))

  implicit def deriveKey[name](implicit name: Name[name]): SwaggerMapper[Key[name]] = fromFunc(PathSpec.key.set(name.string.some))

  implicit val monoidKInstance = new MonoidK[SwaggerMapper] {
    def empty[A]: SwaggerMapper[A] = SwaggerMapper.empty
    def combineK[A](x: SwaggerMapper[A], y: SwaggerMapper[A]): SwaggerMapper[A] = x andThen y
  }

  implicit def monoidInstance[A] = monoidKInstance.algebra[A]
}

final case class MethodDeclare[method](method: OpenApi.Method)
object MethodDeclare {
  implicit val checkGet: MethodDeclare[Get] = MethodDeclare(OpenApi.Method.get)
  implicit val checkPost: MethodDeclare[Post] = MethodDeclare(OpenApi.Method.post)
  implicit val checkDelete: MethodDeclare[Delete] = MethodDeclare(OpenApi.Method.delete)
  implicit val checkPut: MethodDeclare[Put] = MethodDeclare(OpenApi.Method.put)
  implicit val checkOptions: MethodDeclare[Options] = MethodDeclare(OpenApi.Method.options)
  implicit val checkHead: MethodDeclare[Head] = MethodDeclare(OpenApi.Method.head)
  implicit val checkPatch: MethodDeclare[Patch] = MethodDeclare(OpenApi.Method.patch)
}







