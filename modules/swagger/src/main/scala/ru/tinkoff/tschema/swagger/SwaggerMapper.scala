package ru.tinkoff.tschema.swagger

import java.util.{Date, UUID}

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.syntax.option._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.instances.map._
import cats.instances.vector._
import cats.kernel.Semigroup
import cats.{Eval, Monoid, MonoidK}
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.macros.MakerMacro
import ru.tinkoff.tschema.swagger.MkSwagger._
import ru.tinkoff.tschema.swagger.OpenApiParam.In
import ru.tinkoff.tschema.swagger.PathDescription.{DescriptionMap, TypeTarget}
import ru.tinkoff.tschema.swagger.SwaggerBuilder.EmptySwaggerBuilder
import ru.tinkoff.tschema.swagger.SwaggerMapper.derivedParamAtom
import ru.tinkoff.tschema.typeDSL._
import ru.tinkoff.tschema.utils.subsets._
import tofu.optics.chain
import tofu.optics.tags.at
import shapeless.{Lazy, Witness}

import scala.annotation.implicitNotFound
import scala.collection.immutable.TreeMap
import scala.language.higherKinds
@implicitNotFound("Could not find swagger atom for ${T}")
trait SwaggerMapper[T] extends FunctionK[MkSwagger, MkSwagger] {
  self =>
  def mapSpec(spec: PathSpec): PathSpec
  def types: Map[String, DescribedType]
  def auths: TreeMap[String, OpenApiSecurity]

  class Applied(to: SwaggerBuilder) extends SwaggerBuilder {
    def paths: PathSeq  = to.paths.map(mapSpec)
    def types: TypePool = to.types ++ self.types
    def tags            = to.tags
    def auths           = to.auths ++ self.auths
  }

  def apply[A](mkSwagger: MkSwagger[A]): MkSwagger[A] = new Applied(mkSwagger) with MkSwagger[A]

  def to[A](to: SwaggerBuilder): SwaggerBuilder = new Applied(to)

  def andThen[B](other: SwaggerMapper[B]) = new SwaggerMapper[B] {
    def mapSpec(spec: PathSpec): PathSpec = self.mapSpec(other.mapSpec(spec))
    val types                             = self.types ++ other.types
    val auths                             = self.auths ++ other.auths
  }

  def map(f: PathSpec => PathSpec) = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec = f(self.mapSpec(spec))
    def types: Map[String, DescribedType] = self.types
    def auths                             = self.auths
  }

  def ++(that: TraversableOnce[(String, SwaggerType)]) = new SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec       = self.mapSpec(spec)
    def types: Map[String, DescribedType]       = self.types ++ that.map { case (name, typ) => name -> DescribedType(typ) }
    def auths: TreeMap[String, OpenApiSecurity] = self.auths
  }

  def as[U] = self.asInstanceOf[SwaggerMapper[U]]
}

object SwaggerMapper extends SwaggerMapperInstances1 {

  def apply[T](implicit mapper: SwaggerMapper[T]): SwaggerMapper[T] = mapper

  abstract class Empty[T] extends SwaggerMapper[T] {
    def mapSpec(spec: PathSpec): PathSpec       = spec
    def types: Map[String, DescribedType]       = TreeMap.empty
    def auths: TreeMap[String, OpenApiSecurity] = TreeMap.empty
  }

  def empty[T] = new Empty[T] {}

  import OpenApiParam.In

  def fromFunc[T](f: PathSpec => PathSpec) = new Empty[T] {
    override def mapSpec(spec: PathSpec): PathSpec = f(spec)
  }

  def fromTypes[T](tps: Map[String, DescribedType]) = new Empty[T] {
    override def types: Map[String, DescribedType] = tps
  }

  def fromAuth[T](name: String, auth: OpenApiSecurity) = new Empty[T] {
    override def auths = TreeMap(name -> auth)
    override def mapSpec(spec: PathSpec): PathSpec =
      (PathSpec.op >> OpenApiOp.security).update(spec, _ :+ Map(name -> Vector.empty))
  }

  private[swagger] def derivedParam[name, T, param[_, _]](in: In)(
      implicit name: Name[name],
      param: AsOpenApiParam[T]
  ): SwaggerMapper[param[name, T]] =
    derivedParamAtom[name, T, param[name, T]](in)

  private[swagger] def derivedParamAtom[name, T, atom](in: In, flag: Boolean = false)(
      implicit name: Name[name],
      param: AsOpenApiParam[T]
  ): SwaggerMapper[atom] = {
    def makeSingle(p: OpenApiParamInfo, name: String): OpenApiParam =
      OpenApiParam(
        name = name,
        in = in,
        required = p.required,
        schema = p.typ.some,
        allowEmptyValue = flag
      )

    val parameters: List[OpenApiParam] = param match {
      case ps: AsSingleOpenApiParam[T] => List(makeSingle(ps, name.string))
      case pm: AsMultiOpenApiParam[T]  => pm.fields.map(p => makeSingle(p, p.name)).toList
    }

    fromFunc((PathSpec.op >> OpenApiOp.parameters).update(_, _ ++ parameters)) andThen
      fromTypes[atom](param.types)
  }

  implicit def derivePath[path](implicit name: Name[path]) =
    fromFunc[path](_.modPath(name.string +: _))

  implicit def derivePathPrefix[path](implicit name: Name[path]) =
    fromFunc[Prefix[path]](_.modPath(name.string +: _))

  implicit def derivePathWitness[path](implicit name: Name[path]) =
    fromFunc[Witness.Aux[path]](_.modPath(name.string +: _))

  implicit def deriveQueryParam[name: Name, T: AsOpenApiParam] =
    derivedParam[name, T, QueryParam](In.query)

  implicit def deriveOptQueryParams[name: Name, T](implicit ev: AsOpenApiParam[Option[List[T]]]) =
    derivedParamAtom[name, Option[List[T]], QueryParams[name, Option[T]]](In.query)

  implicit def deriveQueryFlag[name: Name]: SwaggerMapper[QueryFlag[name]] =
    derivedParamAtom[name, Option[Boolean], QueryFlag[name]](In.query, flag = true)

  implicit def deriveHeader[name: Name, T: AsSingleOpenApiParam] =
    derivedParam[name, T, Header](In.header)

  implicit def deriveFormField[name: Name, T](implicit asParam: AsOpenApiParam[T]) =
    fromFunc[FormField[name, T]] {
      def param(field: OpenApiParamInfo, name: String) = MakeFormField(name, field.typ, field.required)

      val params = asParam match {
        case ps: AsSingleOpenApiParam[T] => param(ps, Name[name].string)
        case pm: AsMultiOpenApiParam[T]  => pm.fields.reduceMap(p => param(p, p.name))
      }

      (PathSpec.op >> OpenApiOp.requestBody).update(_, 
        _.fold(params.make)(params.add).some
      )
    }

  implicit def deriveCookie[name: Name, T: AsOpenApiParam] =
    derivedParam[name, T, Cookie](In.cookie)

  implicit def deriveAllQuery[x]: SwaggerMapper[AllQuery[x]] = SwaggerMapper.empty

  implicit def derivePathParam[name, T: AsOpenApiParam](implicit name: Name[name]) =
    derivedParam[name, T, Capture](In.path).map(PathSpec.path.update(_, s"{$name}" +: _))

  implicit def deriveReqBody[name, T](implicit content: SwaggerContent[T]): SwaggerMapper[ReqBody[name, T]] =
    fromFunc(
      (PathSpec.op >> OpenApiOp.requestBody).set(_, OpenApiRequestBody.fromTypes(content.content.flatMap(_._2): _*).some)
    ) andThen fromTypes[ReqBody[name, T]](content.collectTypes)

  implicit def deriveMethod[method](implicit methodDeclare: MethodDeclare[method]): SwaggerMapper[method] =
    fromFunc[method](PathSpec.method.update(_, {
      case None           => methodDeclare.method.some
      case meth @ Some(_) => meth
    }))

  implicit def deriveCons[start, end](
      implicit start: SwaggerMapper[start],
      end: SwaggerMapper[end]
  ): SwaggerMapper[start :> end] =
    (start andThen end).as[start :> end]

  private def deriveDesr[T](descr: SwaggerDescription): SwaggerMapper[T] =
    fromFunc((PathSpec.op >> OpenApiOp.description).set(_, descr.some))

  implicit def deriveTag[name](implicit name: Name[name]): SwaggerMapper[Tag[name]] =
    fromFunc((PathSpec.op >> OpenApiOp.tags).update(_, _ :+ name.string))

  implicit def deriveDeprecated: SwaggerMapper[Deprecated] =
    fromFunc((PathSpec.op >> OpenApiOp.deprecated).set(_, true))

  implicit def deriveAs[x, name](implicit internal: Lazy[SwaggerMapper[x]]): SwaggerMapper[As[x, name]] =
    internal.value.as[As[x, name]]

  implicit def deriveKey[name](implicit name: Name[name]): SwaggerMapper[Key[name]] =
    fromFunc(PathSpec.key.set(_, name.string.some))

  implicit def deriveGroup[name](implicit name: Name[name]): SwaggerMapper[Group[name]] =
    fromFunc(PathSpec.groups.update(_, name.string +: _))

  def swaggerAuth[realm: Name, x, T](
      scheme: Option[OpenApiSecurityScheme] = None,
      name: Option[String] = None,
      typ: OpenApiSecurityType = OpenApiSecurityType.http,
      in: Option[OpenApiParam.In] = None
  ): SwaggerMapper[T] =
    fromAuth(Name[realm].string, OpenApiSecurity(`type` = typ, scheme = scheme, in = in, name = name))

  implicit def basicSwaggerAuth[realm: Name, name: Name, x]: SwaggerMapper[BasicAuth[realm, name, x]] =
    swaggerAuth[realm, x, BasicAuth[realm, name, x]](scheme = OpenApiSecurityScheme.basic.some)

  implicit def bearerSwaggerAuth[realm: Name, name: Name, x]: SwaggerMapper[BearerAuth[realm, name, x]] =
    swaggerAuth[realm, x, BearerAuth[realm, name, x]](scheme = OpenApiSecurityScheme.bearer.some)

  implicit def apiKeyAuth[realm: Name, Param <: CanHoldApiKey, name, x](
      implicit
      param: ApiKeyParam[Param, name, x],
      name: Name[name]
  ): SwaggerMapper[ApiKeyAuth[realm, Param]] =
    swaggerAuth[realm, x, ApiKeyAuth[realm, Param]](
      typ = OpenApiSecurityType.apiKey,
      in = param.in.some,
      name = Name[name].string.some
    )

  implicit val monoidKInstance = new MonoidK[SwaggerMapper] {
    def empty[A]: SwaggerMapper[A]                                              = SwaggerMapper.empty
    def combineK[A](x: SwaggerMapper[A], y: SwaggerMapper[A]): SwaggerMapper[A] = x andThen y
  }

  implicit def monoidInstance[A] = monoidKInstance.algebra[A]

  final class MakeFormField(val objType: SwaggerType) {
    def myMediaType: MediaType = "application/x-www-form-urlencoded"

    def make: OpenApiRequestBody =
      OpenApiRequestBody(content = Map(myMediaType -> OpenApiMediaType(schema = objType.some)))

    def add(body: OpenApiRequestBody): OpenApiRequestBody =
      chain(body) >> OpenApiRequestBody.content > at >@ myMediaType > some >>
        OpenApiMediaType.schema > some >@ () update (_ merge objType)
  }

  object MakeFormField {
    def apply(name: String, typ: SwaggerType, required: Boolean): MakeFormField =
      new MakeFormField(
        SwaggerObject(
          required = Eval.now(Vector(name).filter(_ => required)),
          properties = Vector(SwaggerProperty(name, typ = Eval.now(typ), description = None))
        )
      )

    implicit val semigroup: Semigroup[MakeFormField] = (x, y) => new MakeFormField(x.objType merge y.objType)
  }
}

trait SwaggerMapperInstances1 { self: SwaggerMapper.type =>
  implicit def deriveQueryParams[name: Name, T](implicit ev: AsSingleOpenApiParam[List[T]]) =
    derivedParamAtom[name, List[T], QueryParams[name, T]](In.query)
}
