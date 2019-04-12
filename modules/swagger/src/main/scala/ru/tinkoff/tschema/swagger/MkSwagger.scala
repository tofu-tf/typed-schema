package ru.tinkoff.tschema.swagger

import java.util.{Date, UUID}

import akka.http.scaladsl.model.MediaTypes.`application/x-www-form-urlencoded`
import akka.http.scaladsl.model.{MediaType, StatusCode, StatusCodes}
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

trait SwaggerBuilder {
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
  def describe(descriptions: DescriptionMap): SwaggerBuilder =
    new SwaggerBuilder.Describe(this, descriptions)
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
      case spec @ PathSpec(_, _, OpenApiOp(opTags, _, _, _, _, _, _, _, _, _), Some(key)) =>
        import PathDescription.MethodTarget
        val method: MethodTarget => Option[SwaggerDescription] = {
          val paths = opTags.map(tag => descriptions.method(s"$tag.$key")) :+ descriptions.method(key)
          target: PathDescription.MethodTarget => paths.foldLeft(Option.empty[SwaggerDescription]) { (result, path) =>
            (result, path(target)) match {
              case (None, desc@Some(_)) => desc
              case (res, _)             => res
            }
          }
        }

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
trait MkSwagger[T] extends SwaggerBuilder {
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
    macro MakerMacro.makeRouteHNil[macroInterface.type, Def, Unit, SwaggerBuilder]

  def of[Def <: DSLDef](definition: => Def): SwaggerBuilder =
    macro MakerMacro.makeRouteHNilUnit[macroInterface.type, Def, SwaggerBuilder]

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

  implicit def derivedComplete[T](implicit content: SwaggerContent[T]) =
    single[Complete[T]](
      op = OpenApiOp(responses = OpenApiResponses(codes = Map(StatusCodes.OK -> OpenApiResponse.makeMany(content.types: _*)))),
      typeList = TreeMap(content.collectTypes.toSeq: _*)
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
