package ru.tinkoff.tschema.swagger

import OpenApi.defaultMediaTypeVec
import enumeratum.{CirceEnum, Enum, EnumEntry}
import io.circe._
import io.circe.derivation._
import io.circe.derivation.annotations._
import io.circe.syntax._
import tofu.optics.{Contains, Subset}
import tofu.optics.macros.{GenContains, GenSubset}
import ru.tinkoff.tschema.utils.json.CirceKeyEnum
import ru.tinkoff.tschema.utils.json.circeSyntax._
import ru.tinkoff.tschema.utils.json.circeCodecs._
import cats.syntax.option._

import scala.collection.immutable.TreeMap

@JsonCodec(Configuration.encodeOnly)
final case class OpenApi(
    openapi: String = "3.0.0",
    info: OpenApiInfo = OpenApiInfo(),
    servers: Vector[OpenApiServer] = Vector.empty,
    components: OpenApiComponents = OpenApiComponents(),
    paths: OpenApi.PathMap = TreeMap.empty,
    tags: Vector[OpenApiTag] = Vector.empty,
    externalDocs: Option[OpenApiExternalDocs] = None
) {
  def addServer(
      url: String,
      description: Option[String] = None,
      variables: Map[String, OpenApiServerVariable] = Map.empty
  ) =
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

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiInfo(
    title: String = "",
    description: Option[SwaggerDescription] = None,
    termsOfService: Option[String] = None,
    contact: Option[OpenApiContact] = None,
    license: Option[OpeApiLicense] = None,
    version: String = ""
)
object OpenApiInfo

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiComponents(
    schemas: TreeMap[String, DescribedType] = TreeMap.empty,
    securitySchemes: TreeMap[String, OpenApiSecurity] = TreeMap.empty
)

object OpenApiComponents

@JsonCodec(Configuration.encodeOnly)
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
@JsonCodec(Configuration.encodeOnly)
final case class OpenApiSchema()

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiContact(name: Option[String] = None, url: Option[String] = None, email: Option[String] = None)
object OpenApiContact

@JsonCodec(Configuration.encodeOnly)
final case class OpeApiLicense(name: String, url: Option[String] = None)
object OpeApiLicense

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiParam(
    name: String,
    in: OpenApiParam.In,
    description: Option[SwaggerDescription] = None,
    required: Boolean = true,
    schema: Option[SwaggerType] = None,
    deprecated: Boolean = false,
    allowEmptyValue: Boolean = false
)

object OpenApiParam {
  val description: Contains[OpenApiParam, Option[SwaggerDescription]] = GenContains[OpenApiParam](_.description)

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

@JsonCodec
final case class OpenApiServer(
    url: String,
    description: Option[String] = None,
    variables: Map[String, OpenApiServerVariable] = Map.empty
)
object OpenApiServer

@JsonCodec
final case class OpenApiServerVariable(enum: Vector[String], default: String, description: Option[String])
object OpenApiServerVariable

sealed trait SwaggerValue {
  def typeName: String
}

@JsonCodec(Configuration.encodeOnly)
final case class SwaggerStringValue(
    format: Option[OpenApiFormat[SwaggerStringValue]] = None,
    default: Option[String] = None,
    maxLength: Option[Int] = None,
    minLength: Option[Int] = None,
    pattern: Option[String] = None,
    enum: Option[Vector[String]] = None
) extends SwaggerValue {
  def typeName = "string"
}

object SwaggerStringValue {
  val uuidPattern = Seq(8, 4, 4, 4, 12).map(k => s"[0-9a-fA-F]{$k}").mkString("-")
  val timePattern = s"(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(\\.[0-9]+)?(Z)?"
  val uuid        = SwaggerStringValue(pattern = Some(uuidPattern))
  val time        = SwaggerStringValue(pattern = Some(timePattern))
}

@JsonCodec(Configuration.encodeOnly)
final case class SwaggerNumberValue(
    format: Option[OpenApiFormat[SwaggerNumberValue]] = None,
    default: Option[BigDecimal] = None,
    maximum: Option[BigDecimal] = None,
    exclusiveMaximum: Boolean = false,
    minimum: Option[BigDecimal] = None,
    exclusiveMinimum: Boolean = false
) extends SwaggerValue {
  def typeName = "number"
}
object OpenApiNumberValue

@JsonCodec(Configuration.encodeOnly)
final case class SwaggerIntValue(
    format: Option[OpenApiFormat[SwaggerIntValue]] = None,
    default: Option[Int] = None,
    maximum: Option[Int] = None,
    exclusiveMaximum: Option[Boolean] = None,
    minimum: Option[Int] = None,
    exclusiveMinimum: Option[Boolean] = None
) extends SwaggerValue {
  override def typeName = "integer"
}
object SwaggerIntValue

@JsonCodec(Configuration.encodeOnly)
final case class SwaggerBooleanValue(default: Option[Boolean] = None) extends SwaggerValue {
  override def typeName = "boolean"
}
object SwaggerBooleanValue

case object SwaggerFileValue extends SwaggerValue {
  override def typeName = "file"

  implicit val encoder: Encoder.AsObject[SwaggerFileValue.type] = Encoder.AsObject.instance(_ => JsonObject.empty)
}

@JsonCodec(Configuration.encodeOnly)
final case class SwaggerArrayValue(
    items: SwaggerValue,
    default: Option[Vector[Json]] = None,
    collFormat: Option[SwaggerValue.CollectionFormat] = None,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None
) extends SwaggerValue {
  override def typeName = "array"
}
object SwaggerArrayValue

object SwaggerValue {
  val string  = GenSubset[SwaggerValue, SwaggerStringValue]
  val int     = GenSubset[SwaggerValue, SwaggerIntValue]
  val number  = GenSubset[SwaggerValue, SwaggerNumberValue]
  val boolean = GenSubset[SwaggerValue, SwaggerBooleanValue]
  val file    = GenSubset[SwaggerValue, SwaggerFileValue.type]
  val array   = GenSubset[SwaggerValue, SwaggerArrayValue]

  sealed trait CollectionFormat extends EnumEntry

  object CollectionFormat extends Enum[CollectionFormat] with CirceEnum[CollectionFormat] {
    val values = findValues

    case object csv   extends CollectionFormat
    case object ssv   extends CollectionFormat
    case object tsv   extends CollectionFormat
    case object pipes extends CollectionFormat
    case object multi extends CollectionFormat
  }

  private lazy val derivedEncoder: Encoder.AsObject[SwaggerValue] =
    deriveEncoder[SwaggerValue]((x: String) => x, Some("type"))

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
  case object time     extends OpenApiFormat[SwaggerStringValue]
  case object password extends OpenApiFormat[SwaggerStringValue]

  implicit def formatDecoder[T <: SwaggerValue]: Encoder[OpenApiFormat[T]] =
    Encoder.encodeString.contramap[OpenApiFormat[T]](_.toString)
}

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiRequestBody(
    description: Option[String] = None,
    content: Map[MediaType, OpenApiMediaType] = Map.empty,
    required: Boolean = true
)

object OpenApiRequestBody {
  val description: Contains[OpenApiRequestBody, Option[String]] = GenContains[OpenApiRequestBody](_.description)
  val content: Contains[OpenApiRequestBody, Map[MediaType, OpenApiMediaType]] =
    GenContains[OpenApiRequestBody](_.content)

  def fromType(swaggerType: SwaggerType, description: Option[String] = None): OpenApiRequestBody =
    OpenApiRequestBody(
      description = description,
      content = Map(swaggerType.mediaType -> OpenApiMediaType(Some(swaggerType)))
    )

  def fromTypes(swaggerTypes: SwaggerType*): OpenApiRequestBody =
    OpenApiRequestBody(content = swaggerTypes.iterator.map(t => t.mediaType -> OpenApiMediaType(Some(t))).toMap)
}

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiTag(
    name: String,
    description: Option[SwaggerDescription] = None,
    externalDocs: Option[OpenApiExternalDocs] = None
)

object OpenApiTag

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiExternalDocs(description: Option[SwaggerDescription] = None, url: String)

object OpenApiExternalDocs

final case class OpenApiOp(
    tags: Vector[String] = Vector.empty,
    summary: Option[String] = None,
    description: Option[SwaggerDescription] = None,
    externalDocs: Option[OpenApiExternalDocs] = None,
    operationId: Option[String] = None,
    servers: Option[Vector[OpenApiServer]] = None,
    parameters: Vector[OpenApiParam] = Vector.empty,
    requestBody: Option[OpenApiRequestBody] = None,
    responses: OpenApiResponses = OpenApiResponses(),
    security: Vector[Map[String, Vector[String]]] = Vector.empty,
    deprecated: Boolean = false
)

object OpenApiOp {
  implicit lazy val swaggerOpEncoder: Encoder.AsObject[OpenApiOp] = deriveEncoder

  val tags: Contains[OpenApiOp, Vector[String]]                          = GenContains[OpenApiOp](_.tags)
  val summary: Contains[OpenApiOp, Option[String]]                       = GenContains[OpenApiOp](_.summary)
  val description: Contains[OpenApiOp, Option[SwaggerDescription]]       = GenContains[OpenApiOp](_.description)
  val parameters: Contains[OpenApiOp, Vector[OpenApiParam]]              = GenContains[OpenApiOp](_.parameters)
  val requestBody: Contains[OpenApiOp, Option[OpenApiRequestBody]]       = GenContains[OpenApiOp](_.requestBody)
  val responses: Contains[OpenApiOp, OpenApiResponses]                   = GenContains[OpenApiOp](_.responses)
  val security: Contains[OpenApiOp, Vector[Map[String, Vector[String]]]] = GenContains[OpenApiOp](_.security)
  val deprecated: Contains[OpenApiOp, Boolean]                           = GenContains[OpenApiOp](_.deprecated)
}

final case class OpenApiResponses(
    default: Option[OpenApiResponse] = None,
    codes: Map[StatusCode, OpenApiResponse] = Map.empty
)

object OpenApiResponses {
  val default: Contains[OpenApiResponses, Option[OpenApiResponse]]        = GenContains[OpenApiResponses](_.default)
  val codes: Contains[OpenApiResponses, Map[StatusCode, OpenApiResponse]] = GenContains[OpenApiResponses](_.codes)

  implicit lazy val statusCodeEncoder = KeyEncoder.encodeKeyInt

  val mapEnc = Encoder.encodeMapLike[StatusCode, OpenApiResponse, Map]

  implicit lazy val responsesEncoder: Encoder.AsObject[OpenApiResponses] = Encoder.AsObject.instance[OpenApiResponses] {
    resps =>
      val codesObj = mapEnc.encodeObject(resps.codes)
      resps.default match {
        case None          => codesObj
        case Some(default) => codesObj.add("default", default.asJson)
      }
  }
}

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiResponse(
    description: Option[SwaggerDescription] = None,
    content: Map[MediaType, OpenApiMediaType] = Map.empty,
    headers: Map[String, SwaggerValue] = TreeMap.empty
)

object OpenApiResponse {
  def make(description: Option[SwaggerDescription] = None, swaggerType: SwaggerType): OpenApiResponse =
    OpenApiResponse(
      description = description,
      content = Map(swaggerType.mediaType -> OpenApiMediaType(swaggerType.some))
    )

  def makeMany(types: SwaggerType*): OpenApiResponse =
    OpenApiResponse(content = types.iterator.map(t => t.mediaType -> OpenApiMediaType(t.some)).toMap)
}

@JsonCodec(Configuration.encodeOnly)
final case class OpenApiMediaType(schema: Option[SwaggerType] = None, example: Option[Json] = None)
object OpenApiMediaType {
  val schema: Contains[OpenApiMediaType, Option[SwaggerType]] = GenContains[OpenApiMediaType](_.schema)
}
