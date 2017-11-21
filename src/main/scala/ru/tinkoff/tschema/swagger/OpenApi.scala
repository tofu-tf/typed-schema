package ru.tinkoff.tschema.swagger

import OpenApi.jsonMimeType
import akka.http.scaladsl.model.StatusCode
import enumeratum.{CirceEnum, Enum, EnumEntry}
import io.circe._
import io.circe.generic._
import io.circe.generic.semiauto._
import io.circe.syntax._
import monocle.macros.{GenPrism, Lenses}
import ru.tinkoff.tschema.utils.json.CirceKeyEnum
import ru.tinkoff.tschema.utils.json.circeSyntax._
import ru.tinkoff.tschema.utils.json.circeCodecs._

import scala.collection.immutable.TreeMap

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApi(
                          openapi: String = "3.0.0",
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

    case object get extends Method
    case object post extends Method
    case object put extends Method
    case object delete extends Method
    case object head extends Method
    case object options extends Method
  }

  type Path = Map[Method, OpenApiOp]
  type PathMap = TreeMap[String, Path]

  private[tschema] val jsonMimeType = Vector("application/json")
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
final case class OpenApiComponents(schemas: TreeMap[String, DescribedType] = TreeMap.empty)
object OpenApiComponents


@JsonCodec
final case class OpenApiSchema()

@Lenses
@JsonCodec
final case class OpenApiContact(name: Option[String] = None,
                                url: Option[String] = None,
                                email: Option[String] = None)
object OpenApiContact

@Lenses
@JsonCodec
final case class OpeApiLicense(name: String,
                               url: Option[String] = None)
object OpeApiLicense

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiParam(name: String,
                              in: OpenApiParam.In,
                              description: Option[String] = None,
                              required: Boolean = true,
                              schema: Option[SwaggerType] = None)

object OpenApiParam {
  sealed trait In extends EnumEntry
  object In extends Enum[In] with CirceEnum[In] {
    override val values = findValues
    case object query extends In
    case object header extends In
    case object path extends In
    case object cookie extends In
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
final case class OpenApiServerVariable(enum: Vector[String],
                                       default: String,
                                       description: Option[String])
object OpenApiServerVariable


sealed trait SwaggerValue {
  def typeName: String
}

@Lenses
final case class SwaggerStringValue(format: Option[SwaggerFormat[SwaggerStringValue]] = None,
                                    default: Option[String] = None,
                                    maxLength: Option[Int] = None,
                                    minLength: Option[Int] = None,
                                    pattern: Option[String] = None,
                                    enum: Option[Vector[String]] = None) extends SwaggerValue {
  def typeName = "string"
}

object SwaggerStringValue {
  val uuidPattern = Seq(8, 4, 4, 4, 12).map(k => s"[0-9a-fA-F]{$k}").mkString("-")
  val uuid = SwaggerStringValue(pattern = Some(uuidPattern))
}

@Lenses
final case class SwaggerNumberValue(format: Option[SwaggerFormat[SwaggerNumberValue]] = None,
                                    default: Option[BigDecimal] = None,
                                    maximum: Option[BigDecimal] = None,
                                    exclusiveMaximum: Boolean = false,
                                    minimum: Option[BigDecimal] = None,
                                    exclusiveMinimum: Boolean = false) extends SwaggerValue {
  def typeName = "number"
}
object OpenApiNumberValue

@Lenses
final case class SwaggerIntValue(format: Option[SwaggerFormat[SwaggerIntValue]] = None,
                                 default: Option[Int] = None,
                                 maximum: Option[Int] = None,
                                 exclusiveMaximum: Option[Boolean] = None,
                                 minimum: Option[Int] = None,
                                 exclusiveMinimum: Option[Boolean] = None) extends SwaggerValue {
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
                                   maxItems: Option[Int] = None) extends SwaggerValue {
  override def typeName = "array"
}
object SwaggerArrayValue

object SwaggerValue {
  val string = GenPrism[SwaggerValue, SwaggerStringValue]
  val int = GenPrism[SwaggerValue, SwaggerIntValue]
  val number = GenPrism[SwaggerValue, SwaggerNumberValue]
  val boolean = GenPrism[SwaggerValue, SwaggerBooleanValue]
  val file = GenPrism[SwaggerValue, SwaggerFileValue.type]
  val array = GenPrism[SwaggerValue, SwaggerArrayValue]

  sealed trait CollectionFormat extends EnumEntry

  object CollectionFormat extends Enum[CollectionFormat] with CirceEnum[CollectionFormat] {
    val values = findValues

    case object csv extends CollectionFormat
    case object ssv extends CollectionFormat
    case object tsv extends CollectionFormat
    case object pipes extends CollectionFormat
    case object multi extends CollectionFormat
  }

  private lazy val derivedEncoder: ObjectEncoder[SwaggerValue] = deriveEncoder[SwaggerValue].mapJsonObject {
    obj => obj(obj.keys.head).flatMap(_.asObject).getOrElse(JsonObject.empty)
  }
  implicit lazy val encodeSwaggerValue: ObjectEncoder[SwaggerValue] = derivedEncoder.mapObjWithSrc {
    (x, obj) => obj.add("type", Json.fromString(x.typeName))

  }
}

sealed trait SwaggerFormat[T <: SwaggerValue]

object SwaggerFormat {
  case object int32 extends SwaggerFormat[SwaggerIntValue]
  case object int64 extends SwaggerFormat[SwaggerIntValue]
  case object float extends SwaggerFormat[SwaggerNumberValue]
  case object double extends SwaggerFormat[SwaggerNumberValue]
  case object byte extends SwaggerFormat[SwaggerStringValue]
  case object binary extends SwaggerFormat[SwaggerStringValue]
  case object date extends SwaggerFormat[SwaggerStringValue]
  case object dateTime extends SwaggerFormat[SwaggerStringValue]
  case object password extends SwaggerFormat[SwaggerStringValue]

  implicit def formatDecoder[T <: SwaggerValue]: Encoder[SwaggerFormat[T]] =
    Encoder.encodeString.contramap[SwaggerFormat[T]](_.toString)
}

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiRequestBody(
                                     description: Option[String] = None,
                                     content: Map[String, OpenApiMediaType] = Map.empty,
                                     required: Boolean = true
                                   )

object OpenApiRequestBody {
  def apply(description: Option[String], swaggerType: SwaggerType): OpenApiRequestBody =
    OpenApiRequestBody(description = description, content = Map("application/json" -> OpenApiMediaType(Some(swaggerType))))

}

@Lenses
@JsonCodec
final case class OpenApiTag(name: String,
                            description: Option[SwaggerDescription] = None,
                            externalDocs: Option[OpenApiExternalDocs] = None)

object OpenApiTag

@Lenses
@JsonCodec
final case class OpenApiExternalDocs(description: Option[SwaggerDescription] = None,
                                     url: String)

object OpenApiExternalDocs

@Lenses
final case class OpenApiOp(tags: Vector[String] = Vector.empty,
                           summary: Option[String] = None,
                           description: Option[SwaggerDescription] = None,
                           externalDocs: Option[OpenApiExternalDocs] = None,
                           operationId: Option[String] = None,
                           consumes: Vector[String] = jsonMimeType,
                           produces: Vector[String] = jsonMimeType,
                           servers: Vector[OpenApiServer] = Vector.empty,
                           parameters: Vector[OpenApiParam] = Vector.empty,
                           requestBody: Option[OpenApiRequestBody] = None,
                           responses: OpenApiResponses) {
  def addParam(param: OpenApiParam) = copy(parameters = parameters :+ param)
  def addBody(typ: SwaggerType, description: Option[String] = None) =
    copy(requestBody = Some(OpenApiRequestBody(description, typ)))
  def addTag(tag: String) = copy(tags = tags :+ tag)
}

object OpenApiOp {
  implicit lazy val swaggerOpDecoder: ObjectEncoder[OpenApiOp] = deriveEncoder
}

@Lenses
final case class OpenApiResponses(default: Option[OpenApiResponse] = None,
                                  codes: Map[StatusCode, OpenApiResponse] = Map.empty)

object OpenApiResponses {
  implicit lazy val statusCodeEncoder = KeyEncoder.encodeKeyInt.contramap[StatusCode](_.intValue)

  val mapEnc = Encoder.encodeMapLike[StatusCode, OpenApiResponse, Map]

  implicit lazy val responsesEncoder: ObjectEncoder[OpenApiResponses] = ObjectEncoder.instance[OpenApiResponses] {
    resps =>
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
                                 content: Map[String, OpenApiMediaType],
                                 headers: Map[String, SwaggerValue] = Map.empty)

object OpenApiResponse {
  def json(description: Option[SwaggerDescription] = None, swaggerType: SwaggerType): OpenApiResponse =
    OpenApiResponse(description = description, content = Map("application/json" -> OpenApiMediaType(Some(swaggerType))))
}

@Lenses
@JsonCodec(encodeOnly = true)
final case class OpenApiMediaType(schema: Option[SwaggerType] = None,
                                  example: Option[Json] = None)
object OpenApiMediaType





