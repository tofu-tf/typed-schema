package ru.tinkoff.travel.schema.swagger

import java.util.Date

import akka.util.ByteString
import cats.Eval
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import enumeratum.{Enum, EnumEntry}
import io.circe.syntax._
import io.circe.{Json, JsonObject, ObjectEncoder}
import shapeless.labelled.FieldType
import shapeless.{Lazy, _}

import scala.annotation.tailrec
import scala.language.higherKinds
import SwaggerTypeable.Config



sealed trait SwaggerType {
  def merge: PartialFunction[SwaggerType, SwaggerType] = PartialFunction.empty

  def or(that: SwaggerType): SwaggerType =
    merge.applyOrElse(that, Function.const(SwaggerObject(Vector("left" → Eval.now(this), "right" → Eval.now(that)))))
}
class SwaggerPrimitive[Typ <: SwaggerValue](val typ: Typ, val format: Option[SwaggerFormat[Typ]] = None) extends SwaggerType {
  override def merge = {
    case prim: SwaggerPrimitive[_] if typ == prim.typ ⇒
      if (format == prim.format) this else new SwaggerPrimitive(typ)
  }
}

object SwaggerPrimitive {
  case object string extends SwaggerPrimitive(SwaggerStringValue())
  case object number extends SwaggerPrimitive(SwaggerNumberValue())
  case object boolean extends SwaggerPrimitive(SwaggerBooleanValue())
  //  case object `null` extends SwaggerPrimitive(SwaggerN)
  case object integer extends SwaggerPrimitive(SwaggerIntValue(), Some(SwaggerFormat.int32))
  case object long extends SwaggerPrimitive(SwaggerIntValue(), Some(SwaggerFormat.int64))
  case object float extends SwaggerPrimitive(SwaggerNumberValue(), Some(SwaggerFormat.float))
  case object double extends SwaggerPrimitive(SwaggerNumberValue(), Some(SwaggerFormat.double))

  case object byte extends SwaggerPrimitive(SwaggerStringValue(), Some(SwaggerFormat.byte))
  case object binary extends SwaggerPrimitive(SwaggerStringValue(), Some(SwaggerFormat.binary))
  case object date extends SwaggerPrimitive(SwaggerStringValue(), Some(SwaggerFormat.date))
  case object dateTime extends SwaggerPrimitive(SwaggerStringValue(), Some(SwaggerFormat.dateTime))
  case object password extends SwaggerPrimitive(SwaggerStringValue(), Some(SwaggerFormat.password))

}

case class SwaggerEnumeration(alts: Vector[String]) extends SwaggerType {
  override def merge = {
    case SwaggerEnumeration(alts2) ⇒ SwaggerEnumeration((alts ++ alts2).distinct)
  }
}
case class SwaggerArray(items: Eval[SwaggerType]) extends SwaggerType {
  override def merge = {
    case SwaggerArray(items2) ⇒ SwaggerArray(items.map2(items2)(_ or _))
  }
}
case class SwaggerObject(properties: Vector[(String, Eval[SwaggerType])] = Vector.empty, required: Vector[String] = Vector.empty) extends SwaggerType {
  override def merge = {
    case SwaggerObject(p2, req2) ⇒
      val thisMap = properties.toMap
      val thatMap = p2.toMap
      val unionProps = (thisMap -- thatMap.keySet).toVector ++ thatMap.map {
        case (name, prop) ⇒ name -> thisMap.get(name).map(_.map2(prop)(_ or _)).getOrElse(prop)
      }
      val reqs = required.toSet.intersect(req2.toSet).toVector
      SwaggerObject(unionProps, reqs)
  }
}

case class SwaggerRef(name: String, typ: Eval[SwaggerType]) extends SwaggerType {
  override def merge = {
    case SwaggerRef(`name`, t2) ⇒ SwaggerRef(name, typ.map2(typ)(_ or _))
  }
}
case class SwaggerEither(left: Eval[SwaggerType], right: Eval[SwaggerType]) extends SwaggerType

object SwaggerType {

  implicit object encodeSwaggerType extends ObjectEncoder[SwaggerType] {
    def encode(a: SwaggerType): Eval[JsonObject] = a match {
      case pt: SwaggerPrimitive[_] ⇒
        val typeJson = (pt.typ: SwaggerValue).asJsonObject
        val result = pt.format match {
          case None ⇒ typeJson
          case Some(x) ⇒ typeJson.add("format", x.asJson)
        }
        result.pure[Eval]

      case SwaggerEnumeration(alts) ⇒ JsonObject.fromIterable(Vector(
        "type" → Json.fromString("string"),
        "enum" → Json.arr(alts.map(Json.fromString): _*)
      )).pure[Eval]

      case SwaggerRef(name, _) ⇒ JsonObject.singleton(
        "$ref", Json.fromString(s"#/definitions/$name")
      ).pure[Eval]

      case SwaggerArray(items) ⇒ items.flatMap(encode).map(enc ⇒ JsonObject.fromIterable(Vector(
        "type" → Json.fromString("array"),
        "items" → enc.asJson
      )))

      case SwaggerObject(properties, required) ⇒ properties
        .traverse[Eval, (String, Json)] { case (name, prop) ⇒ prop.flatMap(encode).map(name → _.asJson) }
        .map(enc ⇒ JsonObject.fromIterable(Vector(
          "type" → Json.fromString("object"),
          "required" → Json.arr(required.map(Json.fromString): _*),
          "properties" → JsonObject.fromIterable(enc).asJson)))

      case SwaggerEither(left, right) ⇒ left.map2(right)(_ or _).flatMap(encode)
    }

    override def encodeObject(a: SwaggerType): JsonObject = encode(a).value
  }

  implicit class Ops(val typ: SwaggerType) extends AnyVal {
    import scala.::

    @tailrec private def collectTypesImpl(stack: List[SwaggerType], acc: Map[String, SwaggerType]): Map[String, SwaggerType] =
      stack match {
        case Nil ⇒ acc
        case cur :: rest ⇒ cur match {
          case SwaggerRef(name, _) if acc contains name ⇒ collectTypesImpl(rest, acc)
          case SwaggerRef(name, t) ⇒ collectTypesImpl(t.value :: rest, acc + (name → t.value))
          case SwaggerArray(items) ⇒ collectTypesImpl(items.value :: rest, acc)
          case SwaggerObject(props, _) ⇒ collectTypesImpl(props.foldLeft(rest) { case (next, (_, t)) ⇒ t.value :: next }, acc)
          case SwaggerEither(left, right) ⇒ collectTypesImpl(left.value :: right.value :: rest, acc)
          case _ ⇒ collectTypesImpl(rest, acc)
        }
      }

    def collectTypes: Map[String, SwaggerType] = collectTypesImpl(typ :: Nil, Map.empty)
  }
}

trait SwaggerTypeable[T] {
  def swaggerType: SwaggerType
}

object SwaggerTypeable {
  case class Config(propMod: String ⇒ String) {
    def withCamelCase = copy(
      propMod = _.replaceAll(
        "([A-Z]+)([A-Z][a-z])",
        "$1_$2"
      ).replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
    )
  }

  val default = Config(propMod = identity)

  private[schema] implicit class LazyTypeableOps[T](val lazt: Lazy[SwaggerTypeable[T]]) extends AnyVal {
    def later = Eval.later(lazt.value.swaggerType)
  }

  def apply[T](typ: ⇒ SwaggerType) = new SwaggerTypeable[T] {
    lazy val swaggerType = typ
  }

  implicit val swaggerTypeableInteger = SwaggerTypeable[Int](SwaggerPrimitive.integer)
  implicit val swaggerTypeableLong = SwaggerTypeable[Long](SwaggerPrimitive.long)
  implicit val swaggerTypeableFloat = SwaggerTypeable[Float](SwaggerPrimitive.float)
  implicit val swaggerTypeableDouble = SwaggerTypeable[Double](SwaggerPrimitive.double)
  implicit val swaggerTypeableString = SwaggerTypeable[String](SwaggerPrimitive.string)
  implicit val swaggerTypeableByte = SwaggerTypeable[Byte](SwaggerPrimitive.byte)
  implicit val swaggerTypeableBinary = SwaggerTypeable[ByteString](SwaggerPrimitive.binary)
  implicit val swaggerTypeableDateTime = SwaggerTypeable[Date](SwaggerPrimitive.dateTime)
  implicit val swaggerTypeableBoolean = SwaggerTypeable[Boolean](SwaggerPrimitive.boolean)
  implicit val swaggerTypeableBigIng = SwaggerTypeable[BigInt](SwaggerPrimitive.integer)
  implicit val swaggerTypeableBigDecimal = SwaggerTypeable[BigDecimal](SwaggerPrimitive.double)

  implicit val swaggerTypeableJsonObject = SwaggerTypeable[JsonObject](SwaggerObject())

  private def seq[X[_], T](implicit items: Lazy[SwaggerTypeable[T]]) = SwaggerTypeable[X[T]](SwaggerArray(items.later))

  implicit def swaggerVectorTypeable[T: SwaggerTypeable] = seq[Vector, T]
  implicit def swaggerListTypeable[T: SwaggerTypeable] = seq[List, T]
  implicit def swaggerSetTypeable[T: SwaggerTypeable] = seq[Set, T]

  private def coproduct[X[_, _], A, B](implicit left: Lazy[SwaggerTypeable[A]], right: Lazy[SwaggerTypeable[B]]) =
    SwaggerTypeable[X[A, B]](SwaggerEither(left.later, right.later))

  implicit def swaggerEitherTypeable[A: SwaggerTypeable, B: SwaggerTypeable] = coproduct[Either, A, B]

  def genTypeable[T](implicit gen: Lazy[GenericSwaggerTypeable[T]]) = SwaggerTypeable[T](gen.value.swaggerType)
  def genNamedTypeable[T](name: String)(implicit gen: Lazy[GenericSwaggerTypeable[T]]) =
    SwaggerTypeable[T](SwaggerRef(name, gen.later))
  def genWrappedTypeable[T](implicit gen: Lazy[WrappedSwaggerTypeable[T]]) = gen.value

  trait SwaggerTypeableEnum[X <: EnumEntry] {
    self: Enum[X] ⇒

    lazy implicit val swaggerTypeable: SwaggerTypeable[X] = SwaggerTypeable(SwaggerEnumeration(namesToValuesMap.keys.toVector))
  }

  trait SwaggerTypeableEnumeration {
    self: Enumeration ⇒

    lazy implicit val swaggerTypeable: SwaggerTypeable[Value] = SwaggerTypeable(SwaggerEnumeration(values.map(_.toString).toVector))
  }
}

case class GenericSwaggerTypeable[T](swaggerType: SwaggerType) extends SwaggerTypeable[T]

object GenericSwaggerTypeable {
  case class HListProps[L <: HList](props: List[(String, Eval[SwaggerType], Boolean)]) extends AnyVal

  implicit val hNilProps = HListProps[HNil](Nil)

  implicit def hConsReqProps[Name <: Symbol, Value, Tail <: HList]
  (implicit headProp: Lazy[SwaggerTypeable[Value]], tail: HListProps[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.default) =
    HListProps[FieldType[Name, Value] :: Tail]((cfg.propMod(name.value.name), headProp.later, true) :: tail.props)

  implicit def hConsOptProps[Name <: Symbol, Value, Tail <: HList]
  (implicit headProp: Lazy[SwaggerTypeable[Value]], tail: HListProps[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.default) =
    HListProps[FieldType[Name, Option[Value]] :: Tail]((cfg.propMod(name.value.name), headProp.later, false) :: tail.props)

  implicit def genericTypeable[T, L <: HList]
  (implicit lgen: LabelledGeneric.Aux[T, L], list: HListProps[L]): GenericSwaggerTypeable[T] = {
    def required = list.props.filter(_._3).map(_._1).toVector
    def props = list.props.map { case (name, t, _) ⇒ name -> t }.toVector
    def typ = SwaggerObject(props, required)

    GenericSwaggerTypeable[T](typ)
  }
}

case class WrappedSwaggerTypeable[T](swaggerType: SwaggerType) extends SwaggerTypeable[T]

object WrappedSwaggerTypeable {
  case class HListWrap[L <: HList](typ: SwaggerType) extends AnyVal

  implicit def genericTypeable[T, L <: HList]
  (implicit lgen: Generic.Aux[T, L], wrap: HListWrap[L]) =
    WrappedSwaggerTypeable[T](wrap.typ)

  implicit def hListWrap[X]
  (implicit inner: SwaggerTypeable[X]) =
    HListWrap[X :: HNil](inner.swaggerType)
}



