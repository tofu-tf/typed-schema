package ru.tinkoff.tschema.swagger

import java.util.{Date, UUID}

import akka.util.ByteString
import cats.Eval
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import enumeratum.{Enum, EnumEntry}
import io.circe.syntax._
import io.circe.{Encoder, Json, JsonObject, ObjectEncoder}
import shapeless.labelled.FieldType
import shapeless.{Lazy, _}

import scala.annotation.tailrec
import scala.language.higherKinds
import SwaggerTypeable.Config

import scala.reflect.runtime.universe.TypeTag


sealed trait SwaggerType {
  def merge: PartialFunction[SwaggerType, SwaggerType] = PartialFunction.empty

  def or(that: SwaggerType): SwaggerType = SwaggerOneOf(Vector(None -> Eval.now(this), None -> Eval.now(that)))

  def deref: Eval[SwaggerType] = Eval.now(this)

  /**
    * set or change fields description if this is ObjectType
    */
  def describeFields(descrs: (String, String)*): SwaggerType = this

  /**
    * set ot change type description if this is named type
    */
  def describe(descr: String) = this
}

class SwaggerPrimitive[Typ <: SwaggerValue](val typ: Typ, val format: Option[OpenApiFormat[Typ]] = None) extends SwaggerType {
  override def merge = {
    case prim: SwaggerPrimitive[_] if typ == prim.typ =>
      if (format == prim.format) this else new SwaggerPrimitive(typ)
  }
  def mod(f: Typ => Typ) = new SwaggerPrimitive[Typ](f(typ), format)
}

object SwaggerPrimitive {
  case object string extends SwaggerPrimitive(SwaggerStringValue())
  case object uuid extends SwaggerPrimitive(SwaggerStringValue.uuid)
  case object number extends SwaggerPrimitive(SwaggerNumberValue())
  case object boolean extends SwaggerPrimitive(SwaggerBooleanValue())
  //  case object `null` extends SwaggerPrimitive(SwaggerN)
  case object integer extends SwaggerPrimitive(SwaggerIntValue(), Some(OpenApiFormat.int32))
  case object long extends SwaggerPrimitive(SwaggerIntValue(), Some(OpenApiFormat.int64))
  case object float extends SwaggerPrimitive(SwaggerNumberValue(), Some(OpenApiFormat.float))
  case object double extends SwaggerPrimitive(SwaggerNumberValue(), Some(OpenApiFormat.double))

  case object byte extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.byte))
  case object binary extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.binary))
  case object date extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.date))
  case object dateTime extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.dateTime))
  case object password extends SwaggerPrimitive(SwaggerStringValue(), Some(OpenApiFormat.password))
}

final case class SwaggerEnumeration(alts: Vector[String]) extends SwaggerType {
  override def merge = {
    case SwaggerEnumeration(alts2) => SwaggerEnumeration((alts ++ alts2).distinct)
  }
}
final case class SwaggerArray(items: Eval[SwaggerType]) extends SwaggerType {
  override def merge = {
    case SwaggerArray(items2) => SwaggerArray(items.map2(items2)(_ or _))
  }
}

final case class SwaggerProperty(name: String, description: Option[String], typ: Eval[SwaggerType])

final case class SwaggerObject(properties: Vector[SwaggerProperty] = Vector.empty, required: Vector[String] = Vector.empty) extends SwaggerType {
  override def merge = {
    case SwaggerObject(p2, req2) =>
      val thisMap = properties.map(prop => prop.name -> prop).toMap
      val thatMap = p2.map(prop => prop.name -> prop).toMap
      val unionProps = (thisMap -- thatMap.keySet).values.toVector ++ thatMap.values.map {
        case SwaggerProperty(name, descr, prop) => SwaggerProperty(name, descr, thisMap.get(name).map(_.typ.map2(prop)(_ or _)).getOrElse(prop))
      }
      val reqs = required.toSet.intersect(req2.toSet).toVector
      SwaggerObject(unionProps, reqs)
  }
  override def describeFields(descrs: (String, String)*) = {
    val descrMap = descrs.toMap
    copy(properties = properties.map { prop =>
      prop.copy(description = descrMap.get(prop.name).orElse(prop.description))
    })
  }
}

object SwaggerObject {
  def withProps(props: (String, SwaggerType)*) = SwaggerObject(properties = props.map { case (name, typ) => SwaggerProperty(name, None, Eval.now(typ)) }.toVector)
}

final case class SwaggerRef(name: String, descr: Option[String], typ: Eval[SwaggerType]) extends SwaggerType {
  override def merge = {
    case SwaggerRef(`name`, descr2, t2) => SwaggerRef(name, descr.orElse(descr2), typ.map2(typ)(_ or _))
  }
  override def deref = typ.flatMap(_.deref)

  override def describe(description: String) = copy(descr = Some(description))

  override def describeFields(descrs: (String, String)*) = copy(typ = typ.map(_.describeFields(descrs: _*)))
}

final case class SwaggerOneOf(alts: Vector[(Option[String], Eval[SwaggerType])], discriminator: Option[String] = None) extends SwaggerType {
  override def or(that: SwaggerType) = SwaggerOneOf(alts :+ (None -> Eval.now(that)), discriminator)
}

final case class SwaggerMap(value: Eval[SwaggerType]) extends SwaggerType

final case class DescribedType(typ: SwaggerType, description: Option[String] = None)

object DescribedType {
  implicit val encoder: ObjectEncoder[DescribedType] = ObjectEncoder.instance {
    case DescribedType(typ, None)        => typ.asJsonObject
    case DescribedType(typ, Some(descr)) => typ.asJsonObject.add("description", descr.asJson)
  }
}


class SwaggerMapKey[T]

object SwaggerMapKey {
  implicit val stringKey: SwaggerMapKey[String] = new SwaggerMapKey[String]
}

object SwaggerType {

  private def refTo(name: String) = s"#/components/schemas/$name"

  implicit val encodeSwaggerType: ObjectEncoder[SwaggerType] = new ObjectEncoder[SwaggerType] {
    def encode(a: SwaggerType): Eval[JsonObject] = a match {
      case pt: SwaggerPrimitive[_] =>
        val typeJson = (pt.typ: SwaggerValue).asJsonObject
        val result = pt.format match {
          case None    => typeJson
          case Some(x) => typeJson.add("format", x.asJson)
        }
        result.pure[Eval]

      case SwaggerEnumeration(alts) => JsonObject.fromIterable(Vector(
        "type" -> Json.fromString("string"),
        "enum" -> Json.arr(alts.map(Json.fromString): _*)
      )).pure[Eval]

      case SwaggerRef(name, _, _) => JsonObject.singleton(
        "$ref", Json.fromString(refTo(name))
      ).pure[Eval]

      case SwaggerArray(items) => items.flatMap(encode).map(enc => JsonObject(
        "type" -> Json.fromString("array"),
        "items" -> enc.asJson
      ))

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
          JsonObject(
            "type" -> Json.fromString("object"),
            "required" -> Json.arr(required.map(Json.fromString): _*),
            "properties" -> Json.obj(fields: _*))
        }

      case SwaggerOneOf(alts, discriminator) =>
        alts.traverse[Eval, Json] {
          case (nameOpt, etyp) =>
            nameOpt.filter(_ => discriminator.isEmpty).fold(etyp) {
              name => Eval.now(SwaggerObject(Vector(SwaggerProperty(name, None, etyp)), Vector(name)))
            }.flatMap(encode)
            .map(Json.fromJsonObject)
        }
        .map2(alts.flatTraverse {
          case (Some(name), typ) => typ.map {
            case SwaggerRef(ref, _, _) => Vector(name -> refTo(ref))
            case _                     => Vector.empty
          }
          case _                 => Eval.now(Vector.empty)
        }) { (alts, mapping) =>
          val disObj = discriminator.map(name => JsonObject(
            "propertyName" -> Json.fromString(name),
            "mapping" -> mapping.toMap.asJson
          )).asJson
          JsonObject("oneOf" -> Json.arr(alts: _*),
                     "discriminator" -> disObj)
        }

      case SwaggerMap(values) => values.flatMap(encode).map(enc => JsonObject(
        "type" -> Json.fromString("object"),
        "additionalProperties" -> Json.fromJsonObject(enc)
      ))
    }

    override def encodeObject(a: SwaggerType): JsonObject = encode(a).value
  }

  implicit class Ops(val typ: SwaggerType) extends AnyVal {

    import scala.::

    @tailrec private def collectTypesImpl(stack: List[SwaggerType], acc: Map[String, DescribedType]): Map[String, DescribedType] =
      stack match {
        case Nil         => acc
        case cur :: rest => cur match {
          case SwaggerRef(name, _, _) if acc contains name => collectTypesImpl(rest, acc)
          case SwaggerRef(name, descr, t)                  => collectTypesImpl(t.value :: rest, acc + (name -> DescribedType(t.value, descr)))
          case SwaggerArray(items)                         => collectTypesImpl(items.value :: rest, acc)
          case SwaggerObject(props, _)                     => collectTypesImpl(props.map(_.typ.value) ++: rest, acc)
          case SwaggerOneOf(alts, _)                       => collectTypesImpl(alts.map(_._2.value) ++: rest, acc)
          case SwaggerMap(value)                           => collectTypesImpl(value.value :: rest, acc)
          case _                                           => collectTypesImpl(rest, acc)
        }
      }

    def collectTypes: Map[String, DescribedType] = collectTypesImpl(typ :: Nil, Map.empty)
  }
}

trait SwaggerTypeable[T] {
  self =>
  def typ: SwaggerType
  def as[U]: SwaggerTypeable[U] = this.asInstanceOf[SwaggerTypeable[U]]
  def describe(description: String): SwaggerTypeable[T] = new SwaggerTypeable[T] {
    def typ = self.typ.describe(description)
  }
  def describeFields(descriptions: (String, String)*): SwaggerTypeable[T] = new SwaggerTypeable[T] {
    def typ = self.typ.describeFields(descriptions: _*)
  }
  def descr[S <: Symbol, L <: HList]
  (fld: FieldType[S, String])
  (implicit lgen: LabelledGeneric.Aux[T, L], sel: ops.record.Selector[L, S], witness: Witness.Aux[S]) =
    describeFields(witness.value.name -> fld)
}

trait LowLevelSwaggerTypeable {
  @inline final def make[T](t: SwaggerType) = new SwaggerTypeable[T] {
    val typ = t
  }
  @inline final def makeNamed[T](t: SwaggerType, name: String) = new SwaggerTypeable[T] {
    val typ = SwaggerRef(name, None, Eval.later(t))
  }
  @inline final def seq[X[_], T](implicit items: Lazy[SwaggerTypeable[T]]) = make[X[T]](SwaggerArray(items.later))

  final implicit def seqTypeable[T: SwaggerTypeable] = seq[Seq, T]
}

object SwaggerTypeable extends LowLevelSwaggerTypeable with CirceSwaggerInstances {
  def apply[T](implicit typeable: SwaggerTypeable[T]): SwaggerTypeable[T] = typeable

  case class Config(propMod: String => String = identity,
                    altMod: String => String = identity,
                    plainCoproducts: Boolean = false,
                    discriminator: Option[String] = None,
                    namePrefix: String = "") {
    def withCamelCase = copy(
      propMod = _.replaceAll(
        "([A-Z]+)([A-Z][a-z])",
        "$1_$2"
      ).replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
    )

    def withDiscriminator(name: String) = copy(discriminator = Some(name), plainCoproducts = true)
  }

  val defaultConfig = Config()

  private[tschema] implicit class LazyTypeableOps[T](val lazt: Lazy[SwaggerTypeable[T]]) extends AnyVal {
    def later = Eval.later(lazt.value.typ)
  }

  def defer[T](t: => SwaggerType) = new SwaggerTypeable[T] {
    lazy val typ = t
  }


  implicit val swaggerTypeableInteger = make[Int](SwaggerPrimitive.integer)
  implicit val swaggerTypeableLong = make[Long](SwaggerPrimitive.long)
  implicit val swaggerTypeableFloat = make[Float](SwaggerPrimitive.float)
  implicit val swaggerTypeableDouble = make[Double](SwaggerPrimitive.double)
  implicit val swaggerTypeableString = make[String](SwaggerPrimitive.string)
  implicit val swaggerTypeableByte = make[Byte](SwaggerPrimitive.byte)
  implicit val swaggerTypeableBinary = make[ByteString](SwaggerPrimitive.binary)
  implicit val swaggerTypeableDateTime = make[Date](SwaggerPrimitive.dateTime)
  implicit val swaggerTypeableBoolean = make[Boolean](SwaggerPrimitive.boolean)
  implicit val swaggerTypeableBigIng = make[BigInt](SwaggerPrimitive.integer)
  implicit val swaggerTypeableBigDecimal = make[BigDecimal](SwaggerPrimitive.double)
  implicit val swaggerTypeableUUID = make[UUID](new SwaggerPrimitive(SwaggerStringValue.uuid))
  implicit val swaggerTypeableUnit = make[Unit](SwaggerObject())


  implicit val swaggerTypeableJsonObject = make[JsonObject](SwaggerObject())

  implicit def swaggerVectorTypeable[T: SwaggerTypeable] = seq[Vector, T]
  implicit def swaggerListTypeable[T: SwaggerTypeable] = seq[List, T]
  implicit def swaggerSetTypeable[T: SwaggerTypeable] = seq[Set, T]

  implicit def swaggerMapTypeable[K, T](implicit values: Lazy[SwaggerTypeable[T]], keys: SwaggerMapKey[K]) = make[Map[K, T]](SwaggerMap(values.later))

  private def typeSum[X[_, _], A, B](implicit left: Lazy[SwaggerTypeable[A]], right: Lazy[SwaggerTypeable[B]]) =
    make[X[A, B]](SwaggerOneOf(Vector(None -> left.later, None -> right.later)))

  implicit def swaggerEitherTypeable[A: SwaggerTypeable, B: SwaggerTypeable] = typeSum[Either, A, B]

  def genTypeable[T](implicit gen: Lazy[GenericSwaggerTypeable[T]]) = make[T](gen.value.typ)
  def genNamedTypeable[T](name: String)(implicit gen: Lazy[GenericSwaggerTypeable[T]]) = make[T](SwaggerRef(name, None, gen.later))
  def deriveNamedTypeable[T](implicit gen: Lazy[GenericSwaggerTypeable[T]], typeTag: TypeTag[T], config: Config = defaultConfig): SwaggerTypeable[T] =
    genNamedTypeable[T](config.namePrefix + typeTag.tpe.typeSymbol.name.toString)
  def genWrappedTypeable[T](implicit gen: Lazy[WrappedSwaggerTypeable[T]]) = gen.value

  trait SwaggerTypeableEnum[X <: EnumEntry] {
    self: Enum[X] =>

    lazy implicit val swaggerTypeable: SwaggerTypeable[X] = make(SwaggerEnumeration(namesToValuesMap.keys.toVector))
  }

  trait SwaggerTypeableEnumeration {
    self: Enumeration =>

    lazy implicit val swaggerTypeable: SwaggerTypeable[Value] = make(SwaggerEnumeration(values.map(_.toString).toVector))
  }
}

trait GenericSwaggerTypeable[T] extends SwaggerTypeable[T] {
  def typ: SwaggerType
}

object GenericSwaggerTypeable {
  final case class HListProps[L <: HList](props: List[(String, Eval[SwaggerType], Boolean)])
  final case class CoproductAlts[C <: Coproduct](alts: List[(Option[String], Eval[SwaggerType])])

  def apply[T](implicit typ: GenericSwaggerTypeable[T]): GenericSwaggerTypeable[T] = typ

  def make[T](t: SwaggerType) = new GenericSwaggerTypeable[T] {val typ = t}

  implicit val hNilProps = HListProps[HNil](Nil)

  implicit def hConsReqProps[Name <: Symbol, Value, Tail <: HList]
  (implicit headProp: Lazy[SwaggerTypeable[Value]], tail: HListProps[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.defaultConfig) =
    HListProps[FieldType[Name, Value] :: Tail]((cfg.propMod(name.value.name), headProp.later, true) :: tail.props)

  implicit def hConsOptProps[Name <: Symbol, Value, Tail <: HList]
  (implicit headProp: Lazy[SwaggerTypeable[Value]], tail: HListProps[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.defaultConfig) =
    HListProps[FieldType[Name, Option[Value]] :: Tail]((cfg.propMod(name.value.name), headProp.later, false) :: tail.props)

  implicit def genericProductTypeable[T, L <: HList]
  (implicit lgen: LabelledGeneric.Aux[T, L], list: HListProps[L]): GenericSwaggerTypeable[T] = {
    def required = list.props.filter(_._3).map(_._1).toVector

    def props = list.props.map { case (name, t, _) => SwaggerProperty(name, None, t) }.toVector

    def typ = SwaggerObject(props, required)

    make[T](typ)
  }

  implicit val cNilAlts = CoproductAlts[CNil](Nil)

  implicit def cConsAlts[Name <: Symbol, Value, Tail <: Coproduct]
  (implicit headAlt: Lazy[SwaggerTypeable[Value]], tail: CoproductAlts[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.defaultConfig) = {
    val keepName = cfg.discriminator.isDefined || !cfg.plainCoproducts
    val altName = Some(name.value.name).filter(_ => keepName) map cfg.altMod
    CoproductAlts[FieldType[Name, Value] :+: Tail]((altName -> headAlt.later) :: tail.alts)
  }

  implicit def genericSumTypeable[T, C <: Coproduct]
  (implicit gen: LabelledGeneric.Aux[T, C], sum: CoproductAlts[C], cfg: Config = SwaggerTypeable.defaultConfig): GenericSwaggerTypeable[T] =
    make[T](SwaggerOneOf(sum.alts.toVector, cfg.discriminator))
}

case class WrappedSwaggerTypeable[T](typ: SwaggerType) extends SwaggerTypeable[T]

object WrappedSwaggerTypeable {
  case class HListWrap[L <: HList](typ: SwaggerType) extends AnyVal

  implicit def genericTypeable[T, L <: HList]
  (implicit lgen: Generic.Aux[T, L], wrap: HListWrap[L]) =
    WrappedSwaggerTypeable[T](wrap.typ)

  implicit def hListWrap[X]
  (implicit inner: SwaggerTypeable[X]) =
    HListWrap[X :: HNil](inner.typ)
}

sealed trait CirceSwaggerInstances {
  implicit def jsonObjectSwagger = SwaggerTypeable.make(SwaggerObject()).as[JsonObject]
}



