package ru.tinkoff.tschema.swagger

import java.util.{Date, UUID}

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.syntax.option._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.kernel.Semigroup
import cats.{Eval, Monoid, MonoidK}
import ru.tinkoff.tschema.macros.MakerMacro
import ru.tinkoff.tschema.swagger.MkSwagger._
import ru.tinkoff.tschema.swagger.OpenApiParam.In
import ru.tinkoff.tschema.swagger.PathDescription.{DescriptionMap, TypeTarget}
import ru.tinkoff.tschema.swagger.SwaggerBuilder.EmptySwaggerBuilder
import ru.tinkoff.tschema.swagger.SwaggerMapper.derivedParamAtom
import ru.tinkoff.tschema.typeDSL._
import ru.tinkoff.tschema.utils.optics._
import tofu.optics.chain
import tofu.optics.tags.at
import shapeless.{Lazy, Witness}
import ru.tinkoff.tschema.common.Name

import scala.annotation.implicitNotFound
import scala.collection.immutable.TreeMap
import scala.language.higherKinds
import scala.collection.compat._

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

  def ++(that: Iterable[(String, SwaggerType)]) = new SwaggerMapper[T] {
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
    override def auths                             = TreeMap(name -> auth)
    override def mapSpec(spec: PathSpec): PathSpec =
      (PathSpec.op >> OpenApiOp.security).update(spec, _ :+ Map(name -> Vector.empty))
  }

  private[swagger] def derivedParam[name, p, T, param[_, _, _]](in: In)(implicit
      name: Name[name],
      param: AsOpenApiParam[T]
  ): SwaggerMapper[param[name, p, T]] =
    derivedParamAtom[name, T, param[name, p, T]](in)

  private[swagger] def derivedParamAtom[name, T, atom](in: In, flag: Boolean = false)(implicit
      name: Name[name],
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

  implicit def deriveQueryParam[name: Name, p, T: AsOpenApiParam] =
    derivedParam[name, p, T, QueryParamAs](In.query)

  implicit def deriveOptQueryParams[name: Name, p, T](implicit ev: AsOpenApiParam[Option[List[T]]]) =
    derivedParamAtom[name, Option[List[T]], QueryParamsAs[name, p, Option[T]]](In.query)

  implicit def deriveQueryFlag[name: Name, p]: SwaggerMapper[QueryFlagAs[name, p]] =
    derivedParamAtom[name, Option[Boolean], QueryFlagAs[name, p]](In.query, flag = true)

  implicit def deriveHeader[name: Name, p, T: AsSingleOpenApiParam] =
    derivedParam[name, p, T, HeaderAs](In.header)

  implicit def deriveFormField[name: Name, p, T](implicit asParam: AsOpenApiParam[T]) =
    fromFunc[FormFieldAs[name, p, T]] {
      def param(field: OpenApiParamInfo, name: String) = MakeFormField.urlencoded(name, field.typ, field.required)

      val params = asParam match {
        case ps: AsSingleOpenApiParam[T] => param(ps, Name[name].string)
        case pm: AsMultiOpenApiParam[T]  => pm.fields.reduceMap(p => param(p, p.name))
      }

      (PathSpec.op >> OpenApiOp.requestBody).update(_, _.fold(params.make)(params.add).some)
    } andThen fromTypes[FormFieldAs[name, p, T]](asParam.types)

  implicit def deriveMultipartField[name: Name, p, T](implicit asParam: AsOpenApiParam[T]) =
    fromFunc[MultipartFieldAs[name, p, T]] {
      def param(field: OpenApiParamInfo, name: String) = MakeFormField.multipart(name, field.typ, field.required)

      val params = asParam match {
        case ps: AsSingleOpenApiParam[T] => param(ps, Name[name].string)
        case pm: AsMultiOpenApiParam[T]  => pm.fields.reduceMap(p => param(p, p.name))
      }

      (PathSpec.op >> OpenApiOp.requestBody).update(_, _.fold(params.make)(params.add).some)
    } andThen fromTypes[MultipartFieldAs[name, p, T]](asParam.types)

  implicit def deriveCookie[name: Name, p, T: AsOpenApiParam] =
    derivedParam[name, p, T, CookieAs](In.cookie)

  implicit def deriveAllQuery[x]: SwaggerMapper[AllQuery[x]] = SwaggerMapper.empty

  implicit def derivePathParam[name, p, T: AsOpenApiParam](implicit name: Name[name]) =
    derivedParam[name, p, T, CaptureAs](In.path).map(PathSpec.path.update(_, s"{$name}" +: _))

  implicit def deriveReqOptBody[name, p, T](implicit
      content: SwaggerContent[T]
  ): SwaggerMapper[ReqBodyAs[name, p, Option[T]]] = {
    fromFunc(
      (PathSpec.op >> OpenApiOp.requestBody).set(
        _,
        OpenApiRequestBody
          .fromTypes(
            required = false,
            swaggerTypes = content.content.flatMap(_._2): _*
          )
          .some
      )
    ) andThen fromTypes[ReqBodyAs[name, p, Option[T]]](content.collectTypes)
  }

  implicit def deriveMethod[method](implicit methodDeclare: MethodDeclare[method]): SwaggerMapper[method] =
    fromFunc[method](
      PathSpec.method.update(
        _,
        {
          case None           => methodDeclare.method.some
          case meth @ Some(_) => meth
        }
      )
    )

  implicit def deriveCons[start, end](implicit
      start: SwaggerMapper[start],
      end: SwaggerMapper[end]
  ): SwaggerMapper[start :> end] =
    (start andThen end).as[start :> end]

  private def deriveDesr[T](descr: SwaggerDescription): SwaggerMapper[T] =
    fromFunc((PathSpec.op >> OpenApiOp.description).set(_, descr.some))

  implicit def deriveTag[name](implicit name: Name[name]): SwaggerMapper[Tag[name]] =
    fromFunc((PathSpec.op >> OpenApiOp.tags).update(_, _ :+ name.string))

  implicit def deriveDeprecated: SwaggerMapper[Deprecated] =
    fromFunc((PathSpec.op >> OpenApiOp.deprecated).set(_, true))

  implicit def deriveAs[name]: SwaggerMapper[As[name]] = SwaggerMapper.empty

  implicit def deriveKey[name](implicit name: Name[name]): SwaggerMapper[Key[name]] =
    fromFunc(PathSpec.key.set(_, name.string.some))

  implicit def deriveGroup[name](implicit name: Name[name]): SwaggerMapper[Group[name]] =
    fromFunc(PathSpec.groups.update(_, name.string +: _))

  def swaggerAuth[realm: Name, x, T](
      scheme: Option[OpenApiSecurityScheme] = None,
      name: Option[String] = None,
      typ: OpenApiSecurityType = OpenApiSecurityType.http,
      in: Option[OpenApiParam.In] = None,
      flows: TreeMap[String, OpenApiFlow] = TreeMap.empty
  ): SwaggerMapper[T] =
    fromAuth(Name[realm].string, OpenApiSecurity(`type` = typ, scheme = scheme, in = in, name = name, flows = flows))

  implicit def basicSwaggerAuth[realm: Name, name: Name, x]: SwaggerMapper[BasicAuth[realm, name, x]] =
    swaggerAuth[realm, x, BasicAuth[realm, name, x]](scheme = OpenApiSecurityScheme.basic.some)

  implicit def bearerSwaggerAuth[realm: Name, name: Name, x]: SwaggerMapper[BearerAuth[realm, name, x]] =
    swaggerAuth[realm, x, BearerAuth[realm, name, x]](scheme = OpenApiSecurityScheme.bearer.some)

  implicit def apiKeyAuth[realm: Name, Param <: CanHoldApiKey, name, x](implicit
      param: ApiKeyParam[Param, name, x],
      name: Name[name]
  ): SwaggerMapper[ApiKeyAuth[realm, Param]] =
    swaggerAuth[realm, x, ApiKeyAuth[realm, Param]](
      typ = OpenApiSecurityType.apiKey,
      in = param.in.some,
      name = Name[name].string.some
    )

  implicit def oauth2SwaggerAuth[x, skip, conf: ConfigDesc.Aux[*, realm0], name, realm0]
      : SwaggerMapper[OAuth2Auth[skip, x, conf, name]] = {
    val conf = ConfigDesc[conf]
    swaggerAuth[realm0, x, OAuth2Auth[skip, x, conf, name]](
      typ = OpenApiSecurityType.oauth2,
      flows = conf.flows
    )(new Name[realm0](conf.realm))
  }

  implicit val monoidKInstance = new MonoidK[SwaggerMapper] {
    def empty[A]: SwaggerMapper[A]                                              = SwaggerMapper.empty
    def combineK[A](x: SwaggerMapper[A], y: SwaggerMapper[A]): SwaggerMapper[A] = x andThen y
  }

  implicit def monoidInstance[A] = monoidKInstance.algebra[A]

  sealed abstract class MakeFormField(val objType: SwaggerType) {
    def myMediaType: MediaType

    def make: OpenApiRequestBody                          =
      OpenApiRequestBody(content = Map(myMediaType -> OpenApiMediaType(schema = objType.some)))

    def add(body: OpenApiRequestBody): OpenApiRequestBody =
      chain(body) >> OpenApiRequestBody.content > at >@ myMediaType > _some >>
        OpenApiMediaType.schema > _some >@ {} update (_ merge objType)
  }
  final class MakeUrlencodedField(override val objType: SwaggerType) extends MakeFormField(objType) {
    def myMediaType: MediaType = "application/x-www-form-urlencoded"
  }
  final class MakeMultipartField(override val objType: SwaggerType) extends MakeFormField(objType) {
    def myMediaType: MediaType = "multipart/form-data"
  }

  object MakeFormField {
    def urlencoded(name: String, typ: SwaggerType, required: Boolean): MakeUrlencodedField =
      new MakeUrlencodedField(swaggerType(name, typ, required))

    def multipart(name: String, typ: SwaggerType, required: Boolean): MakeMultipartField =
      new MakeMultipartField(swaggerType(name, typ, required))

    private def swaggerType(name: String, typ: SwaggerType, required: Boolean) =
      SwaggerObject(
        required = Eval.now(Vector(name).filter(_ => required)),
        properties = Vector(SwaggerProperty(name, typ = Eval.now(typ), description = None))
      )

    implicit val urlencodedSemigroup: Semigroup[MakeUrlencodedField] = (x, y) =>
      new MakeUrlencodedField(x.objType merge y.objType)
    implicit val multipartSemigroup: Semigroup[MakeMultipartField]   = (x, y) =>
      new MakeMultipartField(x.objType merge y.objType)
  }
}

trait SwaggerMapperInstances1 { self: SwaggerMapper.type =>
  implicit def deriveQueryParams[name: Name, T](implicit ev: AsSingleOpenApiParam[List[T]]) =
    derivedParamAtom[name, List[T], QueryParams[name, T]](In.query)

  implicit def deriveReqBody[name, T](implicit content: SwaggerContent[T]): SwaggerMapper[ReqBody[name, T]] = {
    fromFunc(
      (PathSpec.op >> OpenApiOp.requestBody)
        .set(_, OpenApiRequestBody.fromTypes(required = true, swaggerTypes = content.content.flatMap(_._2): _*).some)
    ) andThen fromTypes[ReqBody[name, T]](content.collectTypes)
  }
}
