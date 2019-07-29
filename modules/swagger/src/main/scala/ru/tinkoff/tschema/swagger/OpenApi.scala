package ru.tinkoff.tschema.swagger

import OpenApi.defaultMediaTypeVec
import enumeratum.{CirceEnum, Enum, EnumEntry}
import io.circe._
import io.circe.generic._
import io.circe.generic.semiauto._
import io.circe.syntax._
import monocle.macros.{GenPrism, Lenses}
import ru.tinkoff.tschema.utils.json.CirceKeyEnum
import ru.tinkoff.tschema.utils.json.circeSyntax._
import ru.tinkoff.tschema.utils.json.circeCodecs._
import cats.syntax.option._

import scala.collection.immutable.TreeMap

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApi(openapi: String = "3.0.0",
                         info: OpenApiInfo = OpenApiInfo(),
                         servers: Vector[OpenApiServer] = Vector.empty,
                         components: OpenApiComponents = OpenApiComponents(),
                         paths: OpenApi.PathMap = TreeMap.empty,
                         tags: Vector[OpenApiTag] = Vector.empty,
                         externalDocs: Option[OpenApiExternalDocs] = None) {
  def addServer(url: String, description: Option[String] = None, variables: Map[String, OpenApiServerVariable] = Map.empty) =
    copy(servers = servers :+ OpenApiServer(url = url, description = description, variables = variables))
}

object OpenApi {
  sealed trait Method extends EnumEntry
  object Method extends Enum[Method] with CirceKeyEnum[Method] {
    val values = findValues

    case object get     extends Method
    case object post    extends Method
    case object put     extends Method
    case object delete  extends Method
    case object head    extends Method
    case object patch   extends Method
    case object options extends Method
  }

  type Path    = Map[Method, OpenApiOp]
  type PathMap = TreeMap[String, Path]

  private[tschema] val defaultMediaTypeVec = Vector(none[MediaType])
}

@Lenses
@JsonCodec
final case class OpenApiInfo(title: String = "",
                             description: Option[SwaggerDescription] = None,
                             termsOfService: Option[String] = None,
                             contact: Option[OpenApiContact] = None,
                             license: Option[OpeApiLicense] = None,
                             version: String = "")
object OpenApiInfo

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiComponents(
    schemas: TreeMap[String, DescribedType] = TreeMap.empty,
    securitySchemes: TreeMap[String, OpenApiSecurity] = TreeMap.empty
)

object OpenApiComponents

@Lenses
@JsonCodec
final case class OpenApiSecurity(
    `type`: OpenApiSecurityType,
    scheme: Option[OpenApiSecurityScheme] = None,
    name: Option[String] = None,
    in: Option[OpenApiParam.In] = None,
    description: Option[SwaggerDescription] = None
)
object OpenApiSecurity

sealed trait OpenApiSecurityType extends EnumEntry
object OpenApiSecurityType extends Enum[OpenApiSecurityType] with CirceEnum[OpenApiSecurityType] {
  val values = findValues

  case object apiKey        extends OpenApiSecurityType
  case object http          extends OpenApiSecurityType
  case object oauth2        extends OpenApiSecurityType
  case object openIdConnect extends OpenApiSecurityType
}

sealed trait OpenApiSecurityScheme extends EnumEntry
object OpenApiSecurityScheme extends Enum[OpenApiSecurityScheme] with CirceEnum[OpenApiSecurityScheme] {
  val values = findValues

  case object basic  extends OpenApiSecurityScheme
  case object bearer extends OpenApiSecurityScheme
}
@JsonCodec
final case class OpenApiSchema()

@Lenses
@JsonCodec
final case class OpenApiContact(name: Option[String] = None, url: Option[String] = None, email: Option[String] = None)
object OpenApiContact

@Lenses
@JsonCodec
final case class OpeApiLicense(name: String, url: Option[String] = None)
object OpeApiLicense

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiParam(name: String,
                              in: OpenApiParam.In,
                              description: Option[SwaggerDescription] = None,
                              required: Boolean = true,
                              schema: Option[SwaggerType] = None,
                              deprecated: Boolean = false,
                              allowEmptyValue: Boolean = false)

object OpenApiParam {
  sealed trait In extends EnumEntry
  object In extends Enum[In] with CirceEnum[In] {
    override val values = findValues
    case object query    extends In
    case object header   extends In
    case object path     extends In
    case object cookie   extends In
    case object formData extends In
  }
}

@Lenses
@JsonCodec
final case class OpenApiServer(url: String,
                               description: Option[String] = None,
                               variables: Map[String, OpenApiServerVariable] = Map.empty)
object OpenApiServer

@Lenses
@JsonCodec
final case class OpenApiServerVariable(enum: Vector[String], default: String, description: Option[String])
object OpenApiServerVariable

sealed trait SwaggerValue {
  def typeName: String
}

@Lenses
final case class SwaggerStringValue(format: Option[OpenApiFormat[SwaggerStringValue]] = None,
                                    default: Option[String] = None,
                                    maxLength: Option[Int] = None,
                                    minLength: Option[Int] = None,
                                    pattern: Option[String] = None,
                                    enum: Option[Vector[String]] = None)
    extends SwaggerValue {
  def typeName = "string"
}

object SwaggerStringValue {
  val uuidPattern = Seq(8, 4, 4, 4, 12).map(k => s"[0-9a-fA-F]{$k}").mkString("-")
  val uuid        = SwaggerStringValue(pattern = Some(uuidPattern))
}

@Lenses
final case class SwaggerNumberValue(format: Option[OpenApiFormat[SwaggerNumberValue]] = None,
                                    default: Option[BigDecimal] = None,
                                    maximum: Option[BigDecimal] = None,
                                    exclusiveMaximum: Boolean = false,
                                    minimum: Option[BigDecimal] = None,
                                    exclusiveMinimum: Boolean = false)
    extends SwaggerValue {
  def typeName = "number"
}
object OpenApiNumberValue

@Lenses
final case class SwaggerIntValue(format: Option[OpenApiFormat[SwaggerIntValue]] = None,
                                 default: Option[Int] = None,
                                 maximum: Option[Int] = None,
                                 exclusiveMaximum: Option[Boolean] = None,
                                 minimum: Option[Int] = None,
                                 exclusiveMinimum: Option[Boolean] = None)
    extends SwaggerValue {
  override def typeName = "integer"
}
object SwaggerIntValue

@Lenses
final case class SwaggerBooleanValue(default: Option[Boolean] = None) extends SwaggerValue {
  override def typeName = "boolean"
}
object SwaggerBooleanValue

case object SwaggerFileValue extends SwaggerValue {
  override def typeName = "file"
}

@Lenses
final case class SwaggerArrayValue(items: SwaggerValue,
                                   default: Option[Vector[Json]] = None,
                                   collFormat: Option[SwaggerValue.CollectionFormat] = None,
                                   minItems: Option[Int] = None,
                                   maxItems: Option[Int] = None)
    extends SwaggerValue {
  override def typeName = "array"
}
object SwaggerArrayValue

object SwaggerValue {
  val string  = GenPrism[SwaggerValue, SwaggerStringValue]
  val int     = GenPrism[SwaggerValue, SwaggerIntValue]
  val number  = GenPrism[SwaggerValue, SwaggerNumberValue]
  val boolean = GenPrism[SwaggerValue, SwaggerBooleanValue]
  val file    = GenPrism[SwaggerValue, SwaggerFileValue.type]
  val array   = GenPrism[SwaggerValue, SwaggerArrayValue]

  sealed trait CollectionFormat extends EnumEntry

  object CollectionFormat extends Enum[CollectionFormat] with CirceEnum[CollectionFormat] {
    val values = findValues

    case object csv   extends CollectionFormat
    case object ssv   extends CollectionFormat
    case object tsv   extends CollectionFormat
    case object pipes extends CollectionFormat
    case object multi extends CollectionFormat
  }

  private lazy val derivedEncoder: Encoder.AsObject[SwaggerValue] = deriveEncoder[SwaggerValue].mapJsonObject { obj =>
    obj(obj.keys.head).flatMap(_.asObject).getOrElse(JsonObject.empty)
  }
  implicit lazy val encodeSwaggerValue: Encoder.AsObject[SwaggerValue] = derivedEncoder.mapObjWithSrc { (x, obj) =>
    obj.add("type", Json.fromString(x.typeName))

  }
}

sealed trait OpenApiFormat[T <: SwaggerValue]

object OpenApiFormat {
  case object int32    extends OpenApiFormat[SwaggerIntValue]
  case object int64    extends OpenApiFormat[SwaggerIntValue]
  case object float    extends OpenApiFormat[SwaggerNumberValue]
  case object double   extends OpenApiFormat[SwaggerNumberValue]
  case object byte     extends OpenApiFormat[SwaggerStringValue]
  case object binary   extends OpenApiFormat[SwaggerStringValue]
  case object base64   extends OpenApiFormat[SwaggerStringValue]
  case object date     extends OpenApiFormat[SwaggerStringValue]
  case object dateTime extends OpenApiFormat[SwaggerStringValue]
  case object password extends OpenApiFormat[SwaggerStringValue]

  implicit def formatDecoder[T <: SwaggerValue]: Encoder[OpenApiFormat[T]] =
    Encoder.encodeString.contramap[OpenApiFormat[T]](_.toString)
}

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiRequestBody(
    description: Option[String] = None,
    content: Map[MediaType, OpenApiMediaType] = Map.empty,
    required: Boolean = true
)

object OpenApiRequestBody {
  def fromType(swaggerType: SwaggerType, description: Option[String] = None): OpenApiRequestBody =
    OpenApiRequestBody(description = description, content = Map(swaggerType.mediaType -> OpenApiMediaType(Some(swaggerType))))

  def fromTypes(swaggerTypes: SwaggerType*): OpenApiRequestBody =
    OpenApiRequestBody(content = swaggerTypes.iterator.map(t => t.mediaType -> OpenApiMediaType(Some(t))).toMap)
}

@Lenses
@JsonCodec
final case class OpenApiTag(name: String,
                            description: Option[SwaggerDescription] = None,
                            externalDocs: Option[OpenApiExternalDocs] = None)

object OpenApiTag

@Lenses
@JsonCodec
final case class OpenApiExternalDocs(description: Option[SwaggerDescription] = None, url: String)

object OpenApiExternalDocs

@Lenses
final case class OpenApiOp(tags: Vector[String] = Vector.empty,
                           summary: Option[String] = None,
                           description: Option[SwaggerDescription] = None,
                           externalDocs: Option[OpenApiExternalDocs] = None,
                           operationId: Option[String] = None,
                           servers: Option[Vector[OpenApiServer]] = None,
                           parameters: Vector[OpenApiParam] = Vector.empty,
                           requestBody: Option[OpenApiRequestBody] = None,
                           responses: OpenApiResponses = OpenApiResponses(),
                           security: Vector[Map[String, Vector[String]]] = Vector.empty)

object OpenApiOp {
  implicit lazy val swaggerOpEncoder: Encoder.AsObject[OpenApiOp] = deriveEncoder
}

@Lenses
final case class OpenApiResponses(default: Option[OpenApiResponse] = None, codes: Map[StatusCode, OpenApiResponse] = Map.empty)

object OpenApiResponses {
  implicit lazy val statusCodeEncoder = KeyEncoder.encodeKeyInt

  val mapEnc = Encoder.encodeMapLike[StatusCode, OpenApiResponse, Map]

  implicit lazy val responsesEncoder: Encoder.AsObject[OpenApiResponses] = Encoder.AsObject.instance[OpenApiResponses] { resps =>
    val codesObj = mapEnc.encodeObject(resps.codes)
    resps.default match {
      case None          => codesObj
      case Some(default) => codesObj.add("default", default.asJson)
    }
  }
}

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiResponse(description: Option[SwaggerDescription] = None,
                                 content: Map[MediaType, OpenApiMediaType],
                                 headers: Map[String, SwaggerValue] = TreeMap.empty)

object OpenApiResponse {
  def make(description: Option[SwaggerDescription] = None, swaggerType: SwaggerType): OpenApiResponse =
    OpenApiResponse(description = description, content = Map(swaggerType.mediaType -> OpenApiMediaType(swaggerType.some)))

  def makeMany(types: SwaggerType*): OpenApiResponse =
    OpenApiResponse(content = types.iterator.map(t => t.mediaType -> OpenApiMediaType(t.typ.some)).toMap)
}

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiMediaType(schema: Option[SwaggerType] = None, example: Option[Json] = None)
object OpenApiMediaType
