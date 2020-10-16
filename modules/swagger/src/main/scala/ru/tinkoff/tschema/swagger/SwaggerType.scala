package ru.tinkoff.tschema.swagger

import cats.Eval
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.vector._
import io.circe._
import io.circe.syntax._
import io.circe.derivation._
import tofu.optics.{Property, Subset, Update}
import tofu.optics.macros.{GenContains, GenSubset}
import ru.tinkoff.tschema.swagger.internal.merge._
import ru.tinkoff.tschema.utils.json.Skippable
import ru.tinkoff.tschema.utils.optics
import shapeless.tag.@@

import scala.annotation.tailrec
import scala.language.higherKinds
import tofu.optics.Contains

sealed trait SwaggerType {
  def merge: PartialFunction[SwaggerType, SwaggerType] = PartialFunction.empty

  def or(that: SwaggerType): SwaggerType = SwaggerOneOf(Vector(None -> Eval.now(this), None -> Eval.now(that)))

  def and(that: SwaggerType): SwaggerType = SwaggerAllOf(Vector(Eval.now(this), Eval.now(that)))

  def deref: Eval[SwaggerType] = Eval.now(this)

  def mediaType: MediaType = "application/json"

  def nameOpt: Option[String] = None

  /** set ot change type description if this is named type
    */
  def describe(descr: String) = this

  def withMediaType(mediaType: MediaType): SwaggerType = SwaggerMedia(this, mediaType)
}

class SwaggerPrimitive[Typ <: SwaggerValue](
    val typ: Typ,
    val format: Option[OpenApiFormat[Typ]] = None,
    override val mediaType: MediaType = "application/json"
) extends SwaggerType {
  override def merge     = {
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
  case object time     extends SwaggerPrimitive(SwaggerStringValue.time)
  case object password extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.password))

  def bin(mediaType: MediaType): SwaggerPrimitive[SwaggerStringValue] =
    new SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.binary), mediaType)

  def base64(mediaType: MediaType): SwaggerPrimitive[SwaggerStringValue] =
    new SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.base64), mediaType)
}

final case class SwaggerEnumeration(alts: Vector[String]) extends SwaggerType {
  override def merge = { case SwaggerEnumeration(alts2) =>
    SwaggerEnumeration((alts ++ alts2).distinct)
  }
}
final case class SwaggerArray(items: Eval[SwaggerType], minLength: Option[Int] = None, maxLength: Option[Int] = None)
    extends SwaggerType {
  override def merge = { case SwaggerArray(items2, min1, max1) =>
    SwaggerArray(
      items.map2(items2)(_ or _),
      mergeOptWith(minLength, min1)(_ min _),
      mergeOptWith(maxLength, max1)(_ max _)
    )
  }
}

final case class SwaggerProperty(name: String, description: Option[String], typ: Eval[SwaggerType])
object SwaggerProperty {
  val description: Contains[SwaggerProperty, Option[String]] = GenContains[SwaggerProperty](_.description)
  val typ: Contains[SwaggerProperty, Eval[SwaggerType]]      = GenContains[SwaggerProperty](_.typ)
}

final case class SwaggerObject(
    properties: Vector[SwaggerProperty] = Vector.empty,
    required: Eval[Vector[String]] = Eval.now(Vector.empty),
    discriminator: Option[String] = None,
) extends SwaggerType {
  override def merge = { case SwaggerObject(p2, req2, disc2) =>
    val thisMap    = properties.map(prop => prop.name -> prop).toMap
    val thatMap    = p2.map(prop => prop.name -> prop).toMap
    val unionProps = (thisMap -- thatMap.keySet).values.toVector ++ thatMap.values.map {
      case SwaggerProperty(name, descr, prop) =>
        SwaggerProperty(name, descr, thisMap.get(name).map(_.typ.map2(prop)(_ or _)).getOrElse(prop))
    }
    val reqs       = required.map2(req2) { (r1, r2) =>
      r1.toSet.intersect(r2.toSet).toVector
    }
    val discr      = discriminator orElse disc2
    SwaggerObject(unionProps, reqs, discr)
  }

  private def updateProps[T](updates: Seq[(String, T)])(update: T => SwaggerProperty => SwaggerProperty) = {
    val updateMap = updates.toMap
    copy(properties = properties.map { prop =>
      updateMap.get(prop.name).fold(prop)(update(_)(prop))
    })
  }

  /** set or change fields description if this is ObjectType
    */
  def describeFields(descrs: (String, String)*) =
    updateProps(descrs)(descr => SwaggerProperty.description.set(_, descr.some))

  /** set or change XML options if fields
    *
    * @param opts
    */
  def xmlFields(opts: (String, SwaggerXMLOptions)*) =
    updateProps(opts)(opt => (sp) => (SwaggerProperty.typ >> optics.eval[SwaggerType]).update(sp, SwaggerXML.wrap(opt)))
}

object SwaggerObject {
  val properties: Contains[SwaggerObject, Vector[SwaggerProperty]] = GenContains[SwaggerObject](_.properties)
  val required: Contains[SwaggerObject, Eval[Vector[String]]]      = GenContains[SwaggerObject](_.required)
  val discriminator: Contains[SwaggerObject, Option[String]]       = GenContains[SwaggerObject](_.discriminator)

  def withProps(props: (String, SwaggerType)*) =
    SwaggerObject(properties = props.map { case (name, typ) => SwaggerProperty(name, None, Eval.now(typ)) }.toVector)
}

final case class SwaggerRef(name: String, descr: Option[String], typ: Eval[SwaggerType]) extends SwaggerType {
  override def merge                   = { case SwaggerRef(`name`, descr2, t2) =>
    SwaggerRef(name, descr.orElse(descr2), typ.map2(typ)(_ or _))
  }
  override def deref                   = typ.flatMap(_.deref)
  override val nameOpt: Option[String] = Some(name)

  override def describe(description: String) = copy(descr = Some(description))
  override def mediaType: MediaType          = typ.value.mediaType
}
object SwaggerRef {
  val name: Contains[SwaggerRef, String]           = GenContains[SwaggerRef](_.name)
  val typ: Contains[SwaggerRef, Eval[SwaggerType]] = GenContains[SwaggerRef](_.typ)
}

final case class SwaggerOneOf(alts: Vector[(Option[String], Eval[SwaggerType])], discriminator: Option[String] = None)
    extends SwaggerType {
  override def or(that: SwaggerType) = SwaggerOneOf(alts :+ (None -> Eval.now(that)), discriminator)
}

final case class SwaggerAllOf(conjs: Vector[Eval[SwaggerType]]) extends SwaggerType {
  override def and(that: SwaggerType) = SwaggerAllOf(conjs :+ Eval.now(that))
}

final case class SwaggerAnyOf(conjs: Vector[Eval[SwaggerType]]) extends SwaggerType

final case class SwaggerMap(value: Eval[SwaggerType]) extends SwaggerType

final case class SwaggerXML(typ: SwaggerType, options: SwaggerXMLOptions) extends SwaggerType {
  override def mediaType: MediaType = "application/xml"
}
object SwaggerXML {
  val typ: Contains[SwaggerXML, SwaggerType] = GenContains[SwaggerXML](_.typ)

  def wrap(opts: SwaggerXMLOptions)(typ: SwaggerType): SwaggerType = typ match {
    case xml: SwaggerXML => wrap(opts)(xml.typ)
    case ref: SwaggerRef => (SwaggerRef.typ >> optics.eval[SwaggerType]).update(ref, wrap(opts) _)
    case _               => SwaggerXML(typ, opts)
  }
}

final case class SwaggerMedia(typ: SwaggerType, override val mediaType: MediaType) extends SwaggerType {
  override def withMediaType(mediaType: MediaType): SwaggerType = SwaggerMedia(typ, mediaType)
}

final case class DescribedType(
    typ: SwaggerType,
    description: Option[SwaggerDescription] = None,
    title: Option[String] = None
)

object DescribedType {
  val typ: Contains[DescribedType, SwaggerType]                        = GenContains[DescribedType](_.typ)
  val description: Contains[DescribedType, Option[SwaggerDescription]] = GenContains[DescribedType](_.description)
  val title: Contains[DescribedType, Option[String]]                   = GenContains[DescribedType](_.title)

  private val additional: Encoder.AsObject[DescribedType] = deriveEncoder.mapJsonObject(_.remove("typ"))
  implicit val encoder: Encoder.AsObject[DescribedType]   = Encoder.AsObject.instance { dt =>
    additional.encodeObject(dt).toIterable.foldRight(dt.typ.asJsonObject) { _ +: _ }
  }
}

class SwaggerMapKey[T]

object SwaggerMapKey {
  implicit val stringKey: SwaggerMapKey[String] = new SwaggerMapKey[String]
}

object SwaggerType {

  private def refTo(name: String) = s"#/components/schemas/$name"

  val refPrism: Subset[SwaggerType, SwaggerRef] = GenSubset[SwaggerType, SwaggerRef]

  val objProp: Property[SwaggerType, SwaggerObject] = new Property[SwaggerType, SwaggerObject] {
    override def set(st: SwaggerType, obj: SwaggerObject): SwaggerType = st match {
      case _: SwaggerObject => obj
      case ref: SwaggerRef  => SwaggerRef.typ.set(ref, Eval.now(obj))
      case xml: SwaggerXML  => (SwaggerXML.typ >> objProp).set(xml, obj)
      case other            => other
    }

    override def narrow(s: SwaggerType): Either[SwaggerType, SwaggerObject] = s match {
      case obj: SwaggerObject => Right(obj)
      case ref: SwaggerRef    => objProp.narrow(ref.typ.value)
      case xml: SwaggerXML    => objProp.narrow(xml.typ)
      case _                  => Left(s)
    }
  }

  implicit val encodeSwaggerType: Encoder.AsObject[SwaggerType] = new Encoder.AsObject[SwaggerType] {
    def encode(a: SwaggerType): Eval[JsonObject] = a match {
      case pt: SwaggerPrimitive[_] =>
        val typeJson = (pt.typ: SwaggerValue).asJsonObject
        val result   = pt.format match {
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
            )
          )
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
          .map(enc =>
            JsonObject(
              "type"      -> Json.fromString("array"),
              "items"     -> enc.asJson,
              "minLength" -> minLength.asJson,
              "maxLength" -> maxLength.asJson
            )
          )

      case SwaggerXML(typ, options) => encode(typ).map(o1 => o1.add("xml", options.asJson))

      case SwaggerObject(properties, required, discr) =>
        properties
          .traverse[Eval, (String, Option[String], JsonObject)] { case SwaggerProperty(name, descr, prop) =>
            prop.flatMap(encode).map(typ => (name, descr, typ.asJsonObject))
          }
          .map { enc =>
            val fields = enc.map {
              case (name, None, obj)        => name -> obj.asJson
              case (name, Some(descr), obj) => name -> obj.add("description", Json.fromString(descr)).asJson
            }

            JsonObject(
              "type"          -> Json.fromString("object"),
              "required"      -> required.value.toNev.fold(Json.Null)(_.asJson),
              "properties"    -> Json.obj(fields: _*),
              "discriminator" -> discr.map(Discriminator(_)).asJson
            )
          }

      case SwaggerOneOf(alts, discriminator) =>
        alts
          .traverse[Eval, Json] { case (nameOpt, etyp) =>
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
            case _                 => Eval.now(Vector.empty)
          }) { (alts, mapping) =>
            val disObj = discriminator.map(Discriminator(_, mapping.toMap)).asJson
            JsonObject("type" -> "object".asJson, "oneOf" -> Json.arr(alts: _*), "discriminator" -> disObj)
          }

      case SwaggerAllOf(conjs) =>
        conjs
          .traverse(_.flatMap(encode).map(Json.fromJsonObject))
          .map(c => JsonObject("allOf" -> Json.arr(c: _*)))

      case SwaggerAnyOf(conjs) =>
        conjs
          .traverse(_.flatMap(encode).map(Json.fromJsonObject))
          .map(c => JsonObject("anyOf" -> Json.arr(c: _*)))

      case SwaggerMap(values)  =>
        values
          .flatMap(encode)
          .map(enc =>
            JsonObject(
              "type"                 -> Json.fromString("object"),
              "additionalProperties" -> Json.fromJsonObject(enc)
            )
          )

      case SwaggerMedia(typ, _) => encode(typ)
    }

    override def encodeObject(a: SwaggerType): JsonObject = encode(a).value
  }

  implicit class Ops(val typ: SwaggerType) extends AnyVal {

    import scala.::

    @tailrec private def collectTypesImpl(
        stack: List[SwaggerType],
        acc: Map[String, DescribedType]
    ): Map[String, DescribedType] =
      stack match {
        case Nil         => acc
        case cur :: rest =>
          cur match {
            case SwaggerRef(name, _, _) if acc contains name => collectTypesImpl(rest, acc)
            case SwaggerRef(name, descr, t)                  =>
              collectTypesImpl(t.value :: rest, acc + (name -> DescribedType(t.value, descr)))
            case SwaggerArray(items, _, _)                   => collectTypesImpl(items.value :: rest, acc)
            case SwaggerObject(props, _, _)                  => collectTypesImpl(props.map(_.typ.value) ++: rest, acc)
            case SwaggerOneOf(alts, _)                       => collectTypesImpl(alts.map(_._2.value) ++: rest, acc)
            case SwaggerAllOf(conjs)                         => collectTypesImpl(conjs.map(_.value) ++: rest, acc)
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
  implicit val codec: Codec.AsObject[SwaggerXMLOptions] = io.circe.derivation.deriveCodec
}

private[tschema] case class Discriminator(propertyName: String, mapping: Map[String, String] = Map.empty)
object Discriminator {
  implicit val codec: Codec.AsObject[Discriminator] = io.circe.derivation.deriveCodec
}
