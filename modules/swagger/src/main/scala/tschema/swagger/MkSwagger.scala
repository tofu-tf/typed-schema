package tschema.swagger

import cats.instances.map._
import cats.syntax.option._
import cats.{Monoid, MonoidK}
import ru.tinkoff.tschema.macros._
import ru.tinkoff.tschema.swagger.MkSwagger._
import ru.tinkoff.tschema.swagger.PathDescription.{DescriptionMap, TypeTarget}
import ru.tinkoff.tschema.swagger.SwaggerBuilder.EmptySwaggerBuilder
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.typeDSL._
import ru.tinkoff.tschema.utils.subsets._
import tofu.optics.functions.vecItems
import tofu.optics.macros.GenContains
import tofu.optics.{Contains, chain}

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
            specs.collect { case PathSpec(_, Some(method), op, _, _) => method -> op }.toMap
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

  def map(f: PathSpec => PathSpec): SwaggerBuilder = new SwaggerBuilder.SMap(this, f)
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
      case spec @ PathSpec(_, _, op, Some(key), groups) =>
        import PathDescription.MethodTarget
        val method: MethodTarget => Option[SwaggerDescription] = {
          val fullKey = (groups :+ key).mkString(".")
          val paths   = op.tags.map(tag => descriptions.method(s"$tag.$fullKey")) :+ descriptions.method(fullKey)
          target: PathDescription.MethodTarget =>
            paths.foldLeft(Option.empty[SwaggerDescription]) { (result, path) =>
              (result, path(target)) match {
                case (None, desc @ Some(_)) => desc
                case (res, _)               => res
              }
            }
        }

        PathSpec.op.update(
          spec,
          (OpenApiOp.description.update(_, method(MethodTarget.Path) orElse _)) andThen
            (OpenApiOp.summary.update(_, method(MethodTarget.Summary) orElse _)) andThen
            (
              (OpenApiOp.parameters >> vecItems[OpenApiParam, OpenApiParam])
                .update(
                  _,
                  param => OpenApiParam.description.update(param, method(MethodTarget.Param(param.name)) orElse _)
                )
              ) andThen
            (
                (oao: OpenApiOp) =>
                  chain(oao) >>
                    OpenApiOp.requestBody > _some >> OpenApiRequestBody.description update (method(
                    MethodTarget.Body
                  ) orElse _)
              )
        )
      case spec => spec
    }
    val types = {
      self.types.map {
        case (name, t) =>
          val typ = descriptions.typ(name)

          val setDescriptions =
            (DescribedType.title.update(_, typ(TypeTarget.Title) orElse _)) andThen
              (DescribedType.description.update(_, typ(TypeTarget.Type) orElse _)) andThen
              (
                (DescribedType.typ >> SwaggerType.objProp >> SwaggerObject.properties >> vecItems[
                  SwaggerProperty,
                  SwaggerProperty
                ]).update(
                  _,
                  prop => SwaggerProperty.description.update(prop, typ(TypeTarget.Field(prop.name)) orElse _)
                )
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

  def addResponse[U](code: StatusCode, description: Option[SwaggerDescription] = None)(
      implicit typeable: SwaggerTypeable[U]
  ) =
    new MkSwagger[U] {
      val paths: PathSeq = self.paths.map(
        (PathSpec.op >> OpenApiOp.responses >> OpenApiResponses.codes)
          .update(_, _ + (code -> OpenApiResponse.make(description = description, swaggerType = typeable.typ)))
      )

      val types: TypePool                         = self.types ++ typeable.typ.collectTypes
      val tags: TagInfo                           = self.tags
      val auths: TreeMap[String, OpenApiSecurity] = self.auths
    }

  def addDescribedResponse(code: StatusCode, typ: SwaggerType, description: Option[SwaggerDescription] = None) =
    new MkSwagger[T] {
      val paths: PathSeq = self.paths.map(
        (PathSpec.op >> OpenApiOp.responses >> OpenApiResponses.codes)
          .update(_, _ + (code -> OpenApiResponse.make(description = description, swaggerType = typ)))
      )

      val types: TypePool                         = self.types ++ typ.collectTypes
      val tags: TagInfo                           = self.tags
      val auths: TreeMap[String, OpenApiSecurity] = self.auths
    }

  def addContent[U](content: SwaggerContent[U]): MkSwagger[T] = {
    val (newPaths, newTypes) = content.content.foldLeft((this.paths, this.types)) {
      case ((paths, types), (status, ot)) =>
        val resp     = ot.fold(OpenApiResponse())(typ => OpenApiResponse.make(swaggerType = typ))
        val addTypes = ot.fold[Map[String, DescribedType]](Map())(_.collectTypes)
        (
          paths.map(
            (PathSpec.op >> OpenApiOp.responses >> OpenApiResponses.codes)
              .update(_, _ + (status -> resp))
          ),
          types ++ addTypes
        )
    }

    new MkSwagger[T] {
      val paths: PathSeq                          = newPaths
      val types: TypePool                         = newTypes
      val tags: TagInfo                           = self.tags
      val auths: TreeMap[String, OpenApiSecurity] = self.auths
    }

  }

  override def map(f: PathSpec => PathSpec): MkSwagger[T] = new SwaggerBuilder.SMap(this, f) with MkSwagger[T]

  override def describe(descriptions: DescriptionMap): MkSwagger[T] =
    new SwaggerBuilder.Describe(this, descriptions) with MkSwagger[T]
}

object MkSwagger {

  def apply[Def <: DSLDef](definition: => Def): SwaggerBuilder =
    macro MakerMacro.makeRouteHNilNoImpl[Skip, macroInterface.type, Def, SwaggerBuilder]

  object macroInterface {
    class ResultPA1[Out] {
      def apply(in: Unit)(key: String, groups: String*)(implicit swagger: MkSwagger[Complete[Out]]): SwaggerBuilder =
        swagger
    }
    def makeResult[F[_], Out]: ResultPA1[Out]                               = new ResultPA1[Out]
    def concatResults(x: SwaggerBuilder, y: SwaggerBuilder): SwaggerBuilder = x ++ y

    def serve[F[_], T](in: Unit) = new ServePA[T](true)

    class ServePA[T](private val in: Boolean) extends AnyVal {
      def apply(f: Unit => SwaggerBuilder)(implicit swagger: SwaggerMapper[T]): SwaggerBuilder = swagger.to(f(()))
    }
  }

  def empty[T]: MkSwagger[T] = new EmptySwaggerBuilder with MkSwagger[T]

  def summon[T](implicit derived: MkSwagger[T]): MkSwagger[T] = derived

  case class PathSpec(
      path: Vector[String],
      method: Option[OpenApi.Method],
      op: OpenApiOp,
      key: Option[String] = None,
      groups: Vector[String] = Vector.empty
  ) {
    def modPath(f: Vector[String] => Vector[String]) = PathSpec.path.update(this, f)
  }
  object PathSpec {
    val path: Contains[PathSpec, Vector[String]]           = GenContains[PathSpec](_.path)
    val method: Contains[PathSpec, Option[OpenApi.Method]] = GenContains[PathSpec](_.method)
    val op: Contains[PathSpec, OpenApiOp]                  = GenContains[PathSpec](_.op)
    val key: Contains[PathSpec, Option[String]]            = GenContains[PathSpec](_.key)
    val groups: Contains[PathSpec, Vector[String]]         = GenContains[PathSpec](_.groups)
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
      op = OpenApiOp(responses = OpenApiResponses(codes = content.content.groupBy(_._1).map {
        case (i, contents) => (i, OpenApiResponse.makeMany(contents.flatMap(_._2): _*))
      })),
      typeList = TreeMap(content.collectTypes.toSeq: _*)
    )

  implicit def deriveJoin[left, right](implicit left: MkSwagger[left], right: MkSwagger[right]) =
    (left ++ right).as[left <|> right]

  implicit def deriveCons[start, end](
      implicit start: SwaggerMapper[start],
      end: MkSwagger[end]
  ): MkSwagger[start :> end] =
    start(end).as[start :> end]

  implicit val monoidKInstance: MonoidK[MkSwagger] = new MonoidK[MkSwagger] {
    def empty[A]: MkSwagger[A]                                      = MkSwagger.empty[A]
    def combineK[A](x: MkSwagger[A], y: MkSwagger[A]): MkSwagger[A] = x ++ y
  }
  implicit def monoidInstance[A]: Monoid[MkSwagger[A]] = monoidKInstance.algebra[A]
}




