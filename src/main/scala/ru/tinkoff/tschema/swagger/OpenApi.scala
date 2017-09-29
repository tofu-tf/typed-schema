package ru.tinkoff.tschema.swagger

import OpenApi.jsonMimeType
import akka.http.scaladsl.model.StatusCode
import enumeratum.{CirceEnum, Enum, EnumEntry}
import io.circe._
import io.circe.generic._
import io.circe.generic.semiauto._
import io.circe.syntax._
import ru.tinkoff.tschema.utils.json.CirceKeyEnum
import ru.tinkoff.tschema.utils.json.circeSyntax._
import ru.tinkoff.tschema.utils.json.circeCodecs._

import scala.collection.immutable.TreeMap

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

@JsonCodec
final case class OpenApiInfo(title: String = "",
                             description: Option[SwaggerDescription] = None,
                             termsOfService: Option[String] = None,
                             contact: Option[OpenApiContact] = None,
                             license: Option[OpeApiLicense] = None,
                             version: String = "")

@JsonCodec(encodeOnly = true)
final case class OpenApiComponents(schemas: TreeMap[String, SwaggerType] = TreeMap.empty)

@JsonCodec
final case class OpenApiSchema()

@JsonCodec
final case class OpenApiContact(name: Option[String] = None,
                                url: Option[String] = None,
                                email: Option[String] = None)

@JsonCodec
final case class OpeApiLicense(name: String,
                               url: Option[String] = None)

final case class SwaggerParam(base: SwaggerParamBase,
                              specific: SwaggerParamSpecific)

@JsonCodec
final case class OpenApiServer(url: String,
                               description: Option[String] = None,
                               variables: Map[String, OpenApiServerVariable] = Map.empty)

@JsonCodec
final case class OpenApiServerVariable(enum: Vector[String],
                                       default: String,
                                       description: Option[String])

sealed trait SwaggerParamSpecific {
  def in: SwaggerParam.In
}

final case class SwaggerParamBody(schema: SwaggerType) extends SwaggerParamSpecific {
  def in = SwaggerParam.In.body
}

final case class SwaggerParamGeneral(in: SwaggerParam.NonBodyIn,
                                     allowEmptyValue: Boolean = false,
                                     value: SwaggerValue) extends SwaggerParamSpecific

object SwaggerParamSpecific {
  implicit lazy val specificParamFormat = new ObjectEncoder[SwaggerParamSpecific] {
    override def encodeObject(a: SwaggerParamSpecific): JsonObject = a match {
      case SwaggerParamBody(schema)                        => JsonObject.fromIterable(Vector(
        "schema" -> schema.asJson,
        "in" -> Json.fromString("body")))
      case SwaggerParamGeneral(in, allowEmptyValue, value) =>
        value.asJsonObject.add("in", in.asJson).add("allowEmptyValue", Json.fromBoolean(allowEmptyValue))
    }
  }
}

@JsonCodec
final case class SwaggerParamBase(name: String,
                                  description: Option[String] = None,
                                  required: Boolean = false)

case object SwaggerParam {
  sealed trait In extends EnumEntry
  sealed trait NonBodyIn extends In

  object In extends Enum[In] with CirceEnum[In] {
    override val values = findValues
    case object query extends NonBodyIn
    case object header extends NonBodyIn
    case object path extends NonBodyIn
    case object formData extends NonBodyIn
    case object cookie extends NonBodyIn
    case object body extends In
  }

  implicit lazy val nonBodyInEncoder: Encoder[SwaggerParam.NonBodyIn] =
    Encoder[SwaggerParam.In].contramap[SwaggerParam.NonBodyIn](identity)

  implicit lazy val swaggerParamEncoder: ObjectEncoder[SwaggerParam] = ObjectEncoder.instance[SwaggerParam] { p =>
    val base = p.base.asJsonObject
    val specific = p.specific.asJsonObject
    JsonObject.fromMap(specific.toMap ++ base.toMap)
  }
}

sealed trait SwaggerValue {
  def typeName: String
}

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
  def uuid = SwaggerStringValue(pattern = Some(uuidPattern))
}

final case class SwaggerNumberValue(format: Option[SwaggerFormat[SwaggerNumberValue]] = None,
                                    default: Option[BigDecimal] = None,
                                    maximum: Option[BigDecimal] = None,
                                    exclusiveMaximum: Boolean = false,
                                    minimum: Option[BigDecimal] = None,
                                    exclusiveMinimum: Boolean = false) extends SwaggerValue {
  def typeName = "number"
}
final case class SwaggerIntValue(format: Option[SwaggerFormat[SwaggerIntValue]] = None,
                                 default: Option[Int] = None,
                                 maximum: Option[Int] = None,
                                 exclusiveMaximum: Boolean = false,
                                 minimum: Option[Int] = None,
                                 exclusiveMinimum: Boolean = false) extends SwaggerValue {
  override def typeName = "integer"
}

final case class SwaggerBooleanValue(default: Option[Boolean] = None) extends SwaggerValue {
  override def typeName = "boolean"
}

case object SwaggerFileValue extends SwaggerValue {
  override def typeName = "file"
}

final case class SwaggerArrayValue(items: SwaggerValue,
                                   default: Option[Vector[Json]] = None,
                                   collFormat: Option[SwaggerValue.CollectionFormat] = None,
                                   minItems: Option[Int] = None,
                                   maxItems: Option[Int] = None) extends SwaggerValue {
  override def typeName = "array"
}

object SwaggerValue {
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

@JsonCodec
final case class OpenApiTag(name: String,
                            description: Option[SwaggerDescription] = None,
                            externalDocs: Option[OpenApiExternalDocs] = None)

@JsonCodec
final case class OpenApiExternalDocs(description: Option[SwaggerDescription] = None,
                                     url: String)

final case class OpenApiOp(tags: Vector[String] = Vector.empty,
                           summary: Option[String] = None,
                           description: Option[SwaggerDescription] = None,
                           externalDocs: Option[OpenApiExternalDocs] = None,
                           operationId: Option[String] = None,
                           consumes: Vector[String] = jsonMimeType,
                           produces: Vector[String] = jsonMimeType,
                           parameters: Vector[SwaggerParam] = Vector.empty,
                           responses: OpenApiResponses) {
  def addParam(param: SwaggerParam) = copy(parameters = parameters :+ param)
  def addTag(tag: String) = copy(tags = tags :+ tag)
}

object OpenApiOp {
  implicit lazy val swaggerOpDecoder: ObjectEncoder[OpenApiOp] = deriveEncoder
}

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

@JsonCodec(encodeOnly = true)
final case class OpenApiResponse(description: Option[SwaggerDescription] = None,
                                 content: Map[String, OpenApiMediaType],
                                 headers: Map[String, SwaggerValue] = Map.empty)

object OpenApiResponse {
  def json(description: Option[SwaggerDescription] = None, swaggerType: SwaggerType): OpenApiResponse =
    OpenApiResponse(description = description, content = Map("application/json" -> OpenApiMediaType(Some(swaggerType))))
}

@JsonCodec(encodeOnly = true)
final case class OpenApiMediaType(schema: Option[SwaggerType] = None,
                                  example: Option[Json] = None)



