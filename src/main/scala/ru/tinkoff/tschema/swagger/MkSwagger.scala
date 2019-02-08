package ru.tinkoff.tschema.swagger

import java.util.{Date, UUID}

import akka.http.scaladsl.model.MediaTypes.`application/x-www-form-urlencoded`
import akka.http.scaladsl.model.{MediaType, StatusCode, StatusCodes}
import akka.util.ByteString
import cats.arrow.FunctionK
import cats.syntax.option._
import cats.syntax.foldable._
import cats.instances.map._
import cats.instances.vector._
import cats.{Eval, Monoid, MonoidK}
import monocle.function.all._
import monocle.macros.Lenses
import monocle.std.option.some
import ru.tinkoff.tschema.akkaHttp.MakerMacro
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.swagger.MkSwagger._
import ru.tinkoff.tschema.swagger.OpenApiParam.In
import ru.tinkoff.tschema.swagger.PathDescription.{DescriptionMap, TypeTarget}
import ru.tinkoff.tschema.swagger.SwaggerBuilder.EmptySwaggerBuilder
import ru.tinkoff.tschema.swagger.SwaggerMapper.derivedParamAtom
import ru.tinkoff.tschema.typeDSL._
import shapeless.{Lazy, Witness}

import scala.annotation.implicitNotFound
import scala.collection.immutable.TreeMap
import scala.language.higherKinds

sealed trait SwaggerBuilder {
  def paths: PathSeq

  def types: TypePool

  def tags: Map[String, String]

  def auths: TreeMap[String, OpenApiSecurity]

  def ++(other: SwaggerBuilder): SwaggerBuilder = new SwaggerBuilder.Concat(this, other)

  def make(info: OpenApiInfo) = {
    val openApiPaths =
      paths.groupBy(_.path).map {
        case (parts, specs) =>
          parts.mkString("/", "/", "") ->
            specs.collect { case PathSpec(_, Some(method), op, _) => method -> op }.toMap
      }
    OpenApi(
      info = info,
      paths = TreeMap(openApiPaths.toSeq: _*),
      components = OpenApiComponents(
        schemas = types,
        securitySchemes = auths
      ),
      tags = tags.map {
        case (name, descr) =>
          OpenApiTag(name = name, description = descr.some)
      }.toVector
    )
  }

  def map(f: PathSpec => PathSpec): SwaggerBuilder           = new SwaggerBuilder.SMap(this, f)
  def describe(descriptions: DescriptionMap): SwaggerBuilder = new SwaggerBuilder.Describe(this, descriptions)
}

object SwaggerBuilder {
  private[swagger] class EmptySwaggerBuilder extends SwaggerBuilder {
    override def paths: PathSeq                          = Vector.empty
    override def types: TypePool                         = TreeMap.empty
    override def tags                                    = Map.empty[String, SwaggerDescription]
    override def auths: TreeMap[String, OpenApiSecurity] = TreeMap.empty
  }

  val empty: SwaggerBuilder = new EmptySwaggerBuilder

  def tags(ts: Map[String, SwaggerDescription]) = new EmptySwaggerBuilder {
    override val tags = ts
  }

  class Concat(left: SwaggerBuilder, right: SwaggerBuilder) extends SwaggerBuilder {
    val paths = left.paths ++ right.paths
    val types = left.types ++ right.types
    val tags  = left.tags ++ right.tags
    val auths = left.auths ++ right.auths
  }

  class SMap(self: SwaggerBuilder, f: PathSpec => PathSpec) extends SwaggerBuilder {
    val paths = self.paths.map(f)
    val types = self.types
    val tags  = self.tags
    val auths = self.auths
  }

  class Describe(self: SwaggerBuilder, descriptions: PathDescription.DescriptionMap) extends SwaggerBuilder {
    val paths = self.paths.map {
      case spec @ PathSpec(_, _, _, Some(key)) =>
        import PathDescription.MethodTarget
        val method = descriptions.method(key)

        PathSpec.op.modify(
          OpenApiOp.description.modify(method(MethodTarget.Path) orElse _) andThen
            OpenApiOp.summary.modify(method(MethodTarget.Summary) orElse _) andThen
            OpenApiOp.parameters
              .composeTraversal(each)
              .modify(param => OpenApiParam.description.modify(method(MethodTarget.Param(param.name)) orElse _)(param)) andThen
            OpenApiOp.requestBody
              .composePrism(some)
              .composeLens(OpenApiRequestBody.description)
              .modify(method(MethodTarget.Body) orElse _)
        )(spec)
      case spec => spec
    }
    val types = {
      self.types.map {
        case (name, t) =>
          val typ = descriptions.typ(name)

          val setDescriptions =
            DescribedType.title.modify(typ(TypeTarget.Title) orElse _) andThen
              DescribedType.description.modify(typ(TypeTarget.Type) orElse _) andThen
              (DescribedType.typ ^|-? SwaggerType.objOpt ^|-> SwaggerObject.properties ^|->> each).modify(
                prop => SwaggerProperty.description.modify(typ(TypeTarget.Field(prop.name)) orElse _)(prop)
              )

          name -> setDescriptions(t)
      }
    }
    val tags = self.tags ++ {
      import PathDescription.Target.Tag
      val allTags = paths.flatMap(_.op.tags).distinct
      allTags.flatMap(key => descriptions(Tag(key)).map(key -> _))
    }
    val auths = self.auths
  }

  implicit val monoidInstance: Monoid[SwaggerBuilder] = new Monoid[SwaggerBuilder] {
    override def empty: SwaggerBuilder                                         = SwaggerBuilder.empty
    override def combine(x: SwaggerBuilder, y: SwaggerBuilder): SwaggerBuilder = x ++ y
  }
}

@implicitNotFound("Could not find swagger result for ${T}")
sealed trait MkSwagger[T] extends SwaggerBuilder {
  self =>
  def as[U]: MkSwagger[U] = self.asInstanceOf[MkSwagger[U]]

  override def ++(other: SwaggerBuilder): MkSwagger[T] = new SwaggerBuilder.Concat(this, other) with MkSwagger[T]

  def addResponse[U](code: StatusCode, description: Option[SwaggerDescription] = None)(implicit typeable: SwaggerTypeable[U]) =
    new MkSwagger[U] {
      val paths: PathSeq = self.paths.map(
        (PathSpec.op ^|-> OpenApiOp.responses ^|-> OpenApiResponses.codes)
          .modify(_ + (code -> OpenApiResponse.make(description = description, swaggerType = typeable.typ)))
      )

      val types: TypePool                         = self.types ++ typeable.typ.collectTypes
      val tags: TagInfo                           = self.tags
      val auths: TreeMap[String, OpenApiSecurity] = self.auths
    }

  override def map(f: PathSpec => PathSpec): MkSwagger[T] = new SwaggerBuilder.SMap(this, f) with MkSwagger[T]

  override def describe(descriptions: DescriptionMap): MkSwagger[T] =
    new SwaggerBuilder.Describe(this, descriptions) with MkSwagger[T]
}

object MkSwagger {

  def apply[Def <: DSLDef](definition: => Def)(impl: Unit): SwaggerBuilder =
    macro MakerMacro.makeRoute[macroInterface.type, Def, Unit, SwaggerBuilder]

  object macroInterface {
    class ResultPA1[Out] {
      def apply(in: Unit)(impl: Unit)(key: String)(implicit swagger: MkSwagger[Complete[Out]]): SwaggerBuilder =
        swagger
    }
    def makeResult[Out]: ResultPA1[Out]                                     = new ResultPA1[Out]
    def concatResults(x: SwaggerBuilder, y: SwaggerBuilder): SwaggerBuilder = x ++ y

    def serve[T](in: Unit) = new ServePA[T](in)

    class ServePA[T](val in: Unit) extends AnyVal {
      def apply(f: Unit => SwaggerBuilder)(implicit swagger: SwaggerMapper[T]): SwaggerBuilder = swagger.to(f(()))
    }
  }

  def empty[T]: MkSwagger[T] = new EmptySwaggerBuilder with MkSwagger[T]

  def apply[T](implicit derived: MkSwagger[T]): MkSwagger[T] = derived

  @Lenses
  case class PathSpec(path: Vector[String], method: Option[OpenApi.Method], op: OpenApiOp, key: Option[String] = None) {
    def modPath(f: Vector[String] => Vector[String]) = PathSpec.path.modify(f)(this)
  }

  type PathSeq = Vector[PathSpec]

  type TypePool = TreeMap[String, DescribedType]

  type TagInfo = Map[String, SwaggerDescription]

  def single[T](op: OpenApiOp, typeList: TreeMap[String, DescribedType]) = new MkSwagger[T] {
    val paths                                   = Vector(PathSpec(Vector.empty, None, op))
    val types                                   = typeList
    val tags: TagInfo                           = Map.empty
    val auths: TreeMap[String, OpenApiSecurity] = TreeMap.empty
  }

  implicit def derivedComplete[T](implicit typ: SwaggerTypeable[T]) =
    single[Complete[T]](
      op = OpenApiOp(
        responses = OpenApiResponses(
          codes = Map(
            StatusCodes.OK -> OpenApiResponse.make(
              swaggerType = typ.typ
            )))),
      typeList = TreeMap(typ.typ.collectTypes.toSeq: _*)
    )

  implicit def deriveJoin[left, right](implicit left: MkSwagger[left], right: MkSwagger[right]) =
    (left ++ right).as[left <|> right]

  implicit def deriveCons[start, end](implicit start: SwaggerMapper[start], end: MkSwagger[end]): MkSwagger[start :> end] =
    start(end).as[start :> end]

  implicit val monoidKInstance: MonoidK[MkSwagger] = new MonoidK[MkSwagger] {
    def empty[A]: MkSwagger[A]                                      = MkSwagger.empty[A]
    def combineK[A](x: MkSwagger[A], y: MkSwagger[A]): MkSwagger[A] = x ++ y
  }
  implicit def monoidInstance[A]: Monoid[MkSwagger[A]] = monoidKInstance.algebra[A]
}


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
      (PathSpec.op ^|-> OpenApiOp.security).modify(_ :+ Map(name -> Vector.empty))(spec)
  }

  private[swagger] def derivedParam[name, T, param[_, _]](in: In)(
      implicit name: Name[name],
      param: AsSingleOpenApiParam[T]
  ): SwaggerMapper[param[name, T]] =
    derivedParamAtom[name, T, param[name, T]](in)

  private[swagger] def derivedParamAtom[name, T, atom](in: In, flag: Boolean = false)(
      implicit name: Name[name],
      param: AsSingleOpenApiParam[T]
  ): SwaggerMapper[atom] =
    fromFunc(
      (PathSpec.op ^|-> OpenApiOp.parameters)
        .modify(
          _ :+ OpenApiParam(name = name.string,
                            in = in,
                            required = param.required,
                            schema = param.typ.some,
                            allowEmptyValue = flag))) andThen
      fromTypes[atom](param.types)

  implicit def derivePath[path](implicit name: Name[path]) =
    fromFunc[path](_.modPath(name.string +: _))

  implicit def derivePathPrefix[path](implicit name: Name[path]) =
    fromFunc[Prefix[path]](_.modPath(name.string +: _))

  implicit def derivePathWitness[path](implicit name: Name[path]) =
    fromFunc[Witness.Aux[path]](_.modPath(name.string +: _))

  implicit def deriveQueryParam[name: Name, T: AsSingleOpenApiParam] =
    derivedParam[name, T, QueryParam](In.query)

  implicit def deriveOptQueryParams[name: Name, T](implicit ev: AsSingleOpenApiParam[Option[List[T]]]) =
    derivedParamAtom[name, Option[List[T]], QueryParams[name, Option[T]]](In.query)

  implicit def deriveQueryFlag[name: Name]: SwaggerMapper[QueryFlag[name]] =
    derivedParamAtom[name, Option[Boolean], QueryFlag[name]](In.query, flag = true)

  implicit def deriveHeader[name: Name, T: AsSingleOpenApiParam] =
    derivedParam[name, T, Header](In.header)

  implicit def deriveFormField[name: Name, T](implicit asParam: AsSingleOpenApiParam[T]) =
    fromFunc[FormField[name, T]] {
      val param = MakeFormField(Name[name].string, asParam.typ, asParam.required)
      (PathSpec.op ^|-> OpenApiOp.requestBody).modify(
        _.fold(param.make)(param.add).some
      )
    }

  implicit def deriveCookie[name: Name, T: AsSingleOpenApiParam] =
    derivedParam[name, T, Cookie](In.cookie)

  implicit def derivePathParam[name, T: AsSingleOpenApiParam](implicit name: Name[name]) =
    derivedParam[name, T, Capture](In.path).map(PathSpec.path.modify(s"{$name}" +: _))

  implicit def deriveReqBody[name, T](implicit typeable: SwaggerTypeable[T]): SwaggerMapper[ReqBody[name, T]] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.requestBody).set(OpenApiRequestBody.fromType(typeable.typ).some)) andThen fromTypes[
      ReqBody[name, T]](typeable.typ.collectTypes)

  implicit def deriveMethod[method](implicit methodDeclare: MethodDeclare[method]): SwaggerMapper[method] =
    fromFunc[method](PathSpec.method.modify {
      case None           => methodDeclare.method.some
      case meth @ Some(_) => meth
    })

  implicit def deriveCons[start, end](implicit start: SwaggerMapper[start],
                                      end: SwaggerMapper[end]): SwaggerMapper[start :> end] =
    (start andThen end).as[start :> end]

  private def deriveDesr[T](descr: SwaggerDescription): SwaggerMapper[T] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.description).set(descr.some))

  implicit def deriveTag[name](implicit name: Name[name]): SwaggerMapper[Tag[name]] =
    fromFunc((PathSpec.op ^|-> OpenApiOp.tags).modify(_ :+ name.string))

  implicit def deriveAs[x, name](implicit internal: Lazy[SwaggerMapper[x]]): SwaggerMapper[As[x, name]] =
    internal.value.as[As[x, name]]

  implicit def deriveKey[name](implicit name: Name[name]): SwaggerMapper[Key[name]] =
    fromFunc(PathSpec.key.set(name.string.some))

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
    swaggerAuth[realm, x, ApiKeyAuth[realm, Param]](typ = OpenApiSecurityType.apiKey,
                                                    in = param.in.some,
                                                    name = Name[name].string.some)

  implicit val monoidKInstance = new MonoidK[SwaggerMapper] {
    def empty[A]: SwaggerMapper[A]                                              = SwaggerMapper.empty
    def combineK[A](x: SwaggerMapper[A], y: SwaggerMapper[A]): SwaggerMapper[A] = x andThen y
  }

  implicit def monoidInstance[A] = monoidKInstance.algebra[A]

  final case class MakeFormField(name: String, typ: SwaggerType, required: Boolean) {
    def myMediaType: MediaType = `application/x-www-form-urlencoded`

    private def objType: SwaggerType = SwaggerObject(
      required = Eval.now(Vector(name).filter(_ => required)),
      properties = Vector(SwaggerProperty(name, typ = Eval.now(typ), description = None))
    )

    def make: OpenApiRequestBody =
      OpenApiRequestBody(content = Map(myMediaType -> OpenApiMediaType(schema = objType.some)))

    def add: OpenApiRequestBody => OpenApiRequestBody =
      (OpenApiRequestBody.content ^|-? index(myMediaType) ^|-> OpenApiMediaType.schema ^<-? some).modify(_ merge objType)
  }
}

final case class MethodDeclare[method](method: OpenApi.Method)
object MethodDeclare {
  implicit val checkGet: MethodDeclare[Get]         = MethodDeclare(OpenApi.Method.get)
  implicit val checkPost: MethodDeclare[Post]       = MethodDeclare(OpenApi.Method.post)
  implicit val checkDelete: MethodDeclare[Delete]   = MethodDeclare(OpenApi.Method.delete)
  implicit val checkPut: MethodDeclare[Put]         = MethodDeclare(OpenApi.Method.put)
  implicit val checkOptions: MethodDeclare[Options] = MethodDeclare(OpenApi.Method.options)
  implicit val checkHead: MethodDeclare[Head]       = MethodDeclare(OpenApi.Method.head)
  implicit val checkPatch: MethodDeclare[Patch]     = MethodDeclare(OpenApi.Method.patch)
}

final case class ApiKeyParam[Param <: CanHoldApiKey, name, x](in: OpenApiParam.In)

object ApiKeyParam {
  implicit def header[name, x]: ApiKeyParam[Header[name, x], name, x]    = ApiKeyParam(OpenApiParam.In.header)
  implicit def query[name, x]: ApiKeyParam[QueryParam[name, x], name, x] = ApiKeyParam(OpenApiParam.In.query)
  implicit def cookie[name, x]: ApiKeyParam[Cookie[name, x], name, x]    = ApiKeyParam(OpenApiParam.In.cookie)
}

trait SwaggerMapperInstances1 { self: SwaggerMapper.type =>
  implicit def deriveQueryParams[name: Name, T](implicit ev: AsSingleOpenApiParam[List[T]]) =
    derivedParamAtom[name, List[T], QueryParams[name, T]](In.query)
}
