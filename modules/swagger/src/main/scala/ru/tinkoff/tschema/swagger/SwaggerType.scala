package ru.tinkoff.tschema.swagger

import cats.Eval
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.traverse._
import io.circe
import io.circe._
import io.circe.syntax._
import io.circe.derivation._
import monocle.{Optional, Prism, Setter}
import monocle.macros.{GenPrism, GenPrismImpl, Lenses}
import ru.tinkoff.tschema.swagger.internal.merge._
import ru.tinkoff.tschema.utils.json.Skippable
import ru.tinkoff.tschema.utils.setters
import shapeless.tag.@@

import scala.annotation.tailrec
import scala.language.higherKinds

sealed trait SwaggerType {
  def merge: PartialFunction[SwaggerType, SwaggerType] = PartialFunction.empty

  def or(that: SwaggerType): SwaggerType = SwaggerOneOf(Vector(None -> Eval.now(this), None -> Eval.now(that)))

  def deref: Eval[SwaggerType] = Eval.now(this)

  def mediaType: MediaType = "application/json"

  def nameOpt: Option[String] = None

  /**
    * set ot change type description if this is named type
    */
  def describe(descr: String) = this

  def withMediaType(mediaType: MediaType): SwaggerType = SwaggerMedia(this, mediaType)
}

class SwaggerPrimitive[Typ <: SwaggerValue](
    val typ: Typ,
    val format: Option[OpenApiFormat[Typ]] = None,
    override val mediaType: MediaType = "application/json"
) extends SwaggerType {
  override def merge = {
    case prim: SwaggerPrimitive[_] if typ == prim.typ =>
      if (format == prim.format) this else new SwaggerPrimitive(typ)
  }
  def mod(f: Typ => Typ) = new SwaggerPrimitive[Typ](f(typ), format)
}


object SwaggerPrimitive {
  case object string  extends SwaggerPrimitive(SwaggerStringValue())
  case object uuid    extends SwaggerPrimitive(SwaggerStringValue.uuid)
  case object number  extends SwaggerPrimitive(SwaggerNumberValue())
  case object boolean extends SwaggerPrimitive(SwaggerBooleanValue())
  //  case object `null` extends SwaggerPrimitive(SwaggerN)
  case object integer extends SwaggerPrimitive(SwaggerIntValue(), Some(OpenApiFormat.int32))
  case object long    extends SwaggerPrimitive(SwaggerIntValue(), Some(OpenApiFormat.int64))
  case object float   extends SwaggerPrimitive(SwaggerNumberValue(), Some(OpenApiFormat.float))
  case object double  extends SwaggerPrimitive(SwaggerNumberValue(), Some(OpenApiFormat.double))

  case object byte     extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.byte))
  case object binary   extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.binary))
  case object date     extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.date))
  case object dateTime extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.dateTime))
  case object password extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.password))

  def bin(mediaType: MediaType): SwaggerPrimitive[SwaggerStringValue] =
    new SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.binary), mediaType)

  def base64(mediaType: MediaType): SwaggerPrimitive[SwaggerStringValue] =
    new SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.base64), mediaType)
}

final case class SwaggerEnumeration(alts: Vector[String]) extends SwaggerType {
  override def merge = {
    case SwaggerEnumeration(alts2) => SwaggerEnumeration((alts ++ alts2).distinct)
  }
}
final case class SwaggerArray(items: Eval[SwaggerType], minLength: Option[Int] = None, maxLength: Option[Int] = None)
    extends SwaggerType {
  override def merge = {
    case SwaggerArray(items2, min1, max1) =>
      SwaggerArray(items.map2(items2)(_ or _), mergeOptWith(minLength, min1)(_ min _), mergeOptWith(maxLength, max1)(_ max _))
  }
}

@Lenses
final case class SwaggerProperty(name: String, description: Option[String], typ: Eval[SwaggerType])

@Lenses
final case class SwaggerObject(properties: Vector[SwaggerProperty] = Vector.empty,
                               required: Eval[Vector[String]] = Eval.now(Vector.empty))
    extends SwaggerType {
  override def merge = {
    case SwaggerObject(p2, req2) =>
      val thisMap = properties.map(prop => prop.name -> prop).toMap
      val thatMap = p2.map(prop => prop.name         -> prop).toMap
      val unionProps = (thisMap -- thatMap.keySet).values.toVector ++ thatMap.values.map {
        case SwaggerProperty(name, descr, prop) =>
          SwaggerProperty(name, descr, thisMap.get(name).map(_.typ.map2(prop)(_ or _)).getOrElse(prop))
      }
      val reqs = required.map2(req2) { (r1, r2) =>
        r1.toSet.intersect(r2.toSet).toVector
      }
      SwaggerObject(unionProps, reqs)
  }

  private def updateProps[T](updates: Seq[(String, T)])(update: T => SwaggerProperty => SwaggerProperty) = {
    val updateMap = updates.toMap
    copy(properties = properties.map { prop =>
      updateMap.get(prop.name).fold(prop)(update(_)(prop))
    })
  }

  /**
    * set or change fields description if this is ObjectType
    */
  def describeFields(descrs: (String, String)*) =
    updateProps(descrs)(descr => SwaggerProperty.description.set(descr.some))

  /**
    * set or change XML options if fields
    *
    * @param opts
    */
  def xmlFields(opts: (String, SwaggerXMLOptions)*) =
    updateProps(opts)(opt => (SwaggerProperty.typ composeSetter setters.eval).modify(SwaggerXML.wrap(opt)))
}

object SwaggerObject {

  def withProps(props: (String, SwaggerType)*) =
    SwaggerObject(properties = props.map { case (name, typ) => SwaggerProperty(name, None, Eval.now(typ)) }.toVector)
}

@Lenses
final case class SwaggerRef(name: String, descr: Option[String], typ: Eval[SwaggerType]) extends SwaggerType {
  override def merge = {
    case SwaggerRef(`name`, descr2, t2) => SwaggerRef(name, descr.orElse(descr2), typ.map2(typ)(_ or _))
  }
  override def deref                   = typ.flatMap(_.deref)
  override val nameOpt: Option[String] = Some(name)

  override def describe(description: String) = copy(descr = Some(description))
  override def mediaType: MediaType          = typ.value.mediaType
}

final case class SwaggerOneOf(alts: Vector[(Option[String], Eval[SwaggerType])], discriminator: Option[String] = None)
    extends SwaggerType {
  override def or(that: SwaggerType) = SwaggerOneOf(alts :+ (None -> Eval.now(that)), discriminator)
}

final case class SwaggerMap(value: Eval[SwaggerType]) extends SwaggerType

@Lenses
final case class SwaggerXML(typ: SwaggerType, options: SwaggerXMLOptions) extends SwaggerType {
  override def mediaType: MediaType = "application/xml"
}

@Lenses
final case class SwaggerMedia(typ: SwaggerType, override val mediaType: MediaType) extends SwaggerType {
  override def withMediaType(mediaType: MediaType): SwaggerType = SwaggerMedia(typ, mediaType)
}

object SwaggerXML {
  def wrap(opts: SwaggerXMLOptions)(typ: SwaggerType): SwaggerType = typ match {
    case xml: SwaggerXML => wrap(opts)(xml.typ)
    case ref: SwaggerRef => (SwaggerRef.typ composeSetter setters.eval).modify(wrap(opts))(ref)
    case _               => SwaggerXML(typ, opts)
  }
}

@Lenses
final case class DescribedType(
    typ: SwaggerType,
    description: Option[SwaggerDescription] = None,
    title: Option[String] = None
)

object DescribedType {
  private val additional: Encoder.AsObject[DescribedType] = deriveEncoder.mapJsonObject(_.remove("typ"))
  implicit val encoder: Encoder.AsObject[DescribedType] = Encoder.AsObject.instance { dt =>
    additional.encodeObject(dt).toIterable.foldRight(dt.typ.asJsonObject) { _ +: _ }
  }
}

class SwaggerMapKey[T]

object SwaggerMapKey {
  implicit val stringKey: SwaggerMapKey[String] = new SwaggerMapKey[String]
}

object SwaggerType {

  private def refTo(name: String) = s"#/components/schemas/$name"

  val refPrism: Prism[SwaggerType, SwaggerRef] = GenPrism[SwaggerType, SwaggerRef]

  val objOpt: Optional[SwaggerType, SwaggerObject] = Optional[SwaggerType, SwaggerObject] {
    case obj: SwaggerObject => Some(obj)
    case ref: SwaggerRef    => objOpt.getOption(ref.typ.value)
    case xml: SwaggerXML    => objOpt.getOption(xml.typ)
    case _                  => None
  }(obj => {
    case _: SwaggerObject => obj
    case ref: SwaggerRef  => SwaggerRef.typ.set(Eval.now(obj))(ref)
    case xml: SwaggerXML  => (SwaggerXML.typ composeOptional objOpt).set(obj)(xml)
    case other            => other
  })

  val setObj: Setter[SwaggerType, SwaggerObject] = objOpt.asSetter

  implicit val encodeSwaggerType: Encoder.AsObject[SwaggerType] = new Encoder.AsObject[SwaggerType] {
    def encode(a: SwaggerType): Eval[JsonObject] = a match {
      case pt: SwaggerPrimitive[_] =>
        val typeJson = (pt.typ: SwaggerValue).asJsonObject
        val result = pt.format match {
          case None    => typeJson
          case Some(x) => typeJson.add("format", x.asJson)
        }
        result.pure[Eval]

      case SwaggerEnumeration(alts) =>
        JsonObject
          .fromIterable(
            Vector(
              "type" -> Json.fromString("string"),
              "enum" -> Json.arr(alts.map(Json.fromString): _*)
            ))
          .pure[Eval]

      case SwaggerRef(name, _, _) =>
        JsonObject
          .singleton(
            "$ref",
            Json.fromString(refTo(name))
          )
          .pure[Eval]

      case SwaggerArray(items, minLength, maxLength) =>
        items
          .flatMap(encode)
          .map(
            enc =>
              JsonObject(
                "type"      -> Json.fromString("array"),
                "items"     -> enc.asJson,
                "minLength" -> minLength.asJson,
                "maxLength" -> maxLength.asJson
            ))

      case SwaggerXML(typ, options) => encode(typ).map(o1 => o1.add("xml", options.asJson))

      case SwaggerObject(properties, required) =>
        properties
          .traverse[Eval, (String, Option[String], JsonObject)] {
            case SwaggerProperty(name, descr, prop) => prop.flatMap(encode).map(typ => (name, descr, typ.asJsonObject))
          }
          .map { enc =>
            val fields = enc.map {
              case (name, None, obj)        => name -> obj.asJson
              case (name, Some(descr), obj) => name -> obj.add("description", Json.fromString(descr)).asJson
            }
            JsonObject("type"       -> Json.fromString("object"),
                       "required"   -> Json.arr(required.value.map(Json.fromString): _*),
                       "properties" -> Json.obj(fields: _*))
          }

      case SwaggerOneOf(alts, discriminator) =>
        alts
          .traverse[Eval, Json] {
            case (nameOpt, etyp) =>
              nameOpt
                .filter(_ => discriminator.isEmpty)
                .fold(etyp) { name =>
                  Eval.now(SwaggerObject(Vector(SwaggerProperty(name, None, etyp)), Eval.now(Vector(name))))
                }
                .flatMap(encode)
                .map(Json.fromJsonObject)
          }
          .map2(alts.flatTraverse {
            case (Some(name), typ) =>
              typ.map {
                case SwaggerRef(ref, _, _) => Vector(name -> refTo(ref))
                case _                     => Vector.empty
              }
            case _ => Eval.now(Vector.empty)
          }) { (alts, mapping) =>
            val disObj = discriminator
              .map(
                name =>
                  JsonObject(
                    "propertyName" -> Json.fromString(name),
                    "mapping"      -> mapping.toMap.asJson
                ))
              .asJson
            JsonObject("type" -> "object".asJson, "oneOf" -> Json.arr(alts: _*), "discriminator" -> disObj)
          }

      case SwaggerMap(values) =>
        values
          .flatMap(encode)
          .map(
            enc =>
              JsonObject(
                "type"                 -> Json.fromString("object"),
                "additionalProperties" -> Json.fromJsonObject(enc)
            ))

      case SwaggerMedia(typ, _) => encode(typ)
    }

    override def encodeObject(a: SwaggerType): JsonObject = encode(a).value
  }

  implicit class Ops(val typ: SwaggerType) extends AnyVal {

    import scala.::

    @tailrec private def collectTypesImpl(stack: List[SwaggerType],
                                          acc: Map[String, DescribedType]): Map[String, DescribedType] =
      stack match {
        case Nil => acc
        case cur :: rest =>
          cur match {
            case SwaggerRef(name, _, _) if acc contains name => collectTypesImpl(rest, acc)
            case SwaggerRef(name, descr, t)                  => collectTypesImpl(t.value :: rest, acc + (name -> DescribedType(t.value, descr)))
            case SwaggerArray(items, _, _)                   => collectTypesImpl(items.value :: rest, acc)
            case SwaggerObject(props, _)                     => collectTypesImpl(props.map(_.typ.value) ++: rest, acc)
            case SwaggerOneOf(alts, _)                       => collectTypesImpl(alts.map(_._2.value) ++: rest, acc)
            case SwaggerMap(value)                           => collectTypesImpl(value.value :: rest, acc)
            case SwaggerXML(wrapped, _)                      => collectTypesImpl(wrapped :: rest, acc)
            case SwaggerMedia(t, _)                          => collectTypesImpl(t :: rest, acc)
            case _                                           => collectTypesImpl(rest, acc)
          }
      }

    def collectTypes: Map[String, DescribedType] = collectTypesImpl(typ :: Nil, Map.empty)
  }
}

case class SwaggerXMLOptions(
    name: Option[String] = None,
    attribute: Boolean @@ Skippable = false,
    prefix: Option[String] = None,
    namespace: Option[String] = None,
    wrapped: Boolean @@ Skippable = false
)

object SwaggerXMLOptions {
  implicit val encoder: Encoder.AsObject[SwaggerXMLOptions] = io.circe.derivation.deriveEncoder
  implicit val decoder: Decoder[SwaggerXMLOptions]       = io.circe.derivation.deriveDecoder
}
