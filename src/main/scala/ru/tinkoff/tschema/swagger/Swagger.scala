package ru.tinkoff.tschema.swagger

import Swagger.jsonMimeType
import akka.http.scaladsl.model.StatusCode
import enumeratum.{CirceEnum, Enum, EnumEntry}
import io.circe._
import io.circe.generic._
import io.circe.generic.semiauto._
import io.circe.syntax._
import ru.tinkoff.tschema.utils.json.CirceKeyEnum
import ru.tinkoff.tschema.utils.json.circeSyntax._

final case class Swagger(swagger: String = "2.0",
                         info: SwaggerInfo,
                         host: Option[String] = None,
                         basePath: Option[String] = None,
                         schemes: Option[Vector[String]] = None,
                         consumes: Vector[String] = jsonMimeType,
                         produces: Vector[String] = jsonMimeType,
                         paths: Swagger.PathMap = Map.empty,
                         definitions: Map[String, SwaggerType] = Map.empty,
                         tags: Vector[SwaggerTag] = Vector.empty,
                         externalDocs: Option[SwaggerExternalDocs] = None)

object Swagger {
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

  type Path = Map[Method, SwaggerOp]
  type PathMap = Map[String, Path]

  private[tschema] val jsonMimeType = Vector("application/json")

  implicit lazy val swaggerEncoder: ObjectEncoder[Swagger] = deriveEncoder
}

@JsonCodec
final case class SwaggerInfo(title: String = "",
                             description: Option[SwaggerDescription] = None,
                             termsOfService: Option[String] = None,
                             contact: Option[SwaggerContact] = None,
                             license: Option[SwaggerLicense] = None,
                             version: String = "")

@JsonCodec
final case class SwaggerContact(name: Option[String] = None,
                                url: Option[String] = None,
                                email: Option[String] = None)

@JsonCodec
final case class SwaggerLicense(name: String,
                                url: Option[String] = None)

final case class SwaggerParam(base: SwaggerParamBase,
                              specific: SwaggerParamSpecific)

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
      case SwaggerParamBody(schema) => JsonObject.fromIterable(Vector(
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

  private lazy val derivedEncoder: ObjectEncoder[SwaggerValue] = deriveEncoder[SwaggerValue].mapJsonObject{
    obj => obj(obj.fields.head).flatMap(_.asObject).getOrElse(JsonObject.empty)
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
final case class SwaggerTag(name: String,
                            description: Option[SwaggerDescription] = None,
                            externalDocs: Option[SwaggerExternalDocs] = None)

@JsonCodec
final case class SwaggerExternalDocs(description: Option[SwaggerDescription] = None,
                                     url: String)

final case class SwaggerOp(tags: Vector[String] = Vector.empty,
                           summary: Option[String] = None,
                           description: Option[SwaggerDescription] = None,
                           externalDocs: Option[SwaggerExternalDocs] = None,
                           operationId: Option[String] = None,
                           consumes: Vector[String] = jsonMimeType,
                           produces: Vector[String] = jsonMimeType,
                           parameters: Vector[SwaggerParam] = Vector.empty,
                           responses: SwaggerResponses) {
  def addParam(param: SwaggerParam) = copy(parameters = parameters :+ param)
  def addTag(tag: String) = copy(tags = tags :+ tag)
}

object SwaggerOp {
  implicit lazy val swaggerOpDecoder: ObjectEncoder[SwaggerOp] = deriveEncoder
}

final case class SwaggerResponses(default: Option[SwaggerResponse] = None,
                                  codes: Map[StatusCode, SwaggerResponse] = Map.empty)

object SwaggerResponses {
  implicit lazy val statusCodeEncoder = KeyEncoder.encodeKeyInt.contramap[StatusCode](_.intValue)

  val mapEnc = Encoder.encodeMapLike[Map, StatusCode, SwaggerResponse]

  implicit lazy val responsesEncoder: ObjectEncoder[SwaggerResponses] = ObjectEncoder.instance[SwaggerResponses] {
    resps =>
      val codesObj = mapEnc.encodeObject(resps.codes)
      resps.default match {
        case None => codesObj
        case Some(default) => codesObj.add("default", default.asJson)
      }
  }
}

final case class SwaggerResponse(description: Option[SwaggerDescription] = None,
                                 schema: SwaggerType,
                                 headers: Map[String, SwaggerValue] = Map.empty)

object SwaggerResponse {
  implicit lazy val responseEncoder: Encoder[SwaggerResponse] = deriveEncoder
}



