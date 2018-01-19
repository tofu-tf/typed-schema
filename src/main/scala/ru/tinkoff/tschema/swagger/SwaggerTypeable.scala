package ru.tinkoff.tschema.swagger

import java.util.{Date, UUID}

import akka.util.ByteString
import cats.Eval
import enumeratum.{Enum, EnumEntry}
import io.circe.JsonObject
import shapeless.labelled.FieldType
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, LabelledGeneric, Lazy, Witness, ops}
import SwaggerTypeable.Config
import scala.reflect.runtime.universe.TypeTag

trait SwaggerTypeable[T] {
  self =>
  def typ: SwaggerType
  def as[U]: SwaggerTypeable[U] = this.asInstanceOf[SwaggerTypeable[U]]

  private def updateTyp(f: SwaggerType => SwaggerType): SwaggerTypeable[T] =
    SwaggerTypeable.make[T](f(self.typ))

  def describe(description: String): SwaggerTypeable[T] = updateTyp(_.describe(description))

  def describeFields(descriptions: (String, String)*): SwaggerTypeable[T] =
    updateTyp(SwaggerType.setObj.modify(_.describeFields(descriptions: _*)))

  def xml(
    name: Option[String] = None,
    attribute: Boolean = false,
    prefix: Option[String] = None,
    namespace: Option[String] = None,
    wrapped: Boolean = false): SwaggerTypeable[T] =
    updateTyp(SwaggerXML.wrap(SwaggerXMLOptions(
      name = name,
      attribute = attribute,
      prefix = prefix,
      namespace = namespace,
      wrapped = wrapped)))

  def xmlFields(fieldOpts: (String, SwaggerXMLOptions)*) =
    updateTyp(SwaggerType.setObj.modify(_.xmlFields(fieldOpts: _*)))

  //Safe versions
  def descr[S <: Symbol, L <: HList]
  (fld: FieldType[S, String])
    (implicit lgen: LabelledGeneric.Aux[T, L], sel: ops.record.Selector[L, S], witness: Witness.Aux[S]) =
    describeFields(witness.value.name -> fld)

  def xmlFld[S <: Symbol, L <: HList](fld: FieldType[S, SwaggerXMLOptions])
    (implicit lgen: LabelledGeneric.Aux[T, L], sel: ops.record.Selector[L, S], witness: Witness.Aux[S]) =
    xmlFields(witness.value.name -> fld)
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

  private implicit class DescribeTypeOps(val typ: SwaggerType) extends AnyVal {
    def describeWith(descr: Option[String]): SwaggerType = descr.fold(typ)(description => typ.describe(description))
  }

  def apply[T](implicit typ: GenericSwaggerTypeable[T]): GenericSwaggerTypeable[T] = typ

  def make[T](t: SwaggerType) = new GenericSwaggerTypeable[T] {
    val typ = t
  }

  implicit val hNilProps = HListProps[HNil](Nil)

  implicit def hConsReqProps[Name <: Symbol, Value, Tail <: HList]
  (implicit headProp: Lazy[SwaggerTypeable[Value]], tail: HListProps[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.defaultConfig) =
    HListProps[FieldType[Name, Value] :: Tail]((cfg.propMod(name.value.name), headProp.later, true) :: tail.props)

  implicit def hConsOptProps[Name <: Symbol, Value, Tail <: HList]
  (implicit headProp: Lazy[SwaggerTypeable[Value]], tail: HListProps[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.defaultConfig) =
    HListProps[FieldType[Name, Option[Value]] :: Tail]((cfg.propMod(name.value.name), headProp.later, false) :: tail.props)

  implicit def genericProductTypeable[T, L <: HList]
  (implicit lgen: LabelledGeneric.Aux[T, L],
    list: HListProps[L],
    descr: DescribeTypeable[T] = DescribeTypeable.empty[T]): GenericSwaggerTypeable[T] = {
    def required = list.props.filter(_._3).map(_._1).toVector

    def props = list.props.map { case (name, t, _) => SwaggerProperty(name, descr.element(name), t) }.toVector

    make[T](SwaggerObject(props, required).describeWith(descr.whole))
  }

  implicit val cNilAlts = CoproductAlts[CNil](Nil)

  implicit def cConsAlts[Name <: Symbol, Value, Tail <: Coproduct]
  (implicit headAlt: Lazy[SwaggerTypeable[Value]], tail: CoproductAlts[Tail], name: Witness.Aux[Name], cfg: Config = SwaggerTypeable.defaultConfig) = {
    val keepName = cfg.discriminator.isDefined || !cfg.plainCoproducts
    val altName = Some(name.value.name).filter(_ => keepName) map cfg.altMod
    CoproductAlts[FieldType[Name, Value] :+: Tail]((altName -> headAlt.later) :: tail.alts)
  }

  implicit def genericSumTypeable[T, C <: Coproduct]
  (implicit gen: LabelledGeneric.Aux[T, C],
    sum: CoproductAlts[C],
    cfg: Config = SwaggerTypeable.defaultConfig,
    descr: DescribeTypeable[T] = DescribeTypeable.empty[T]): GenericSwaggerTypeable[T] =
    make[T](
      SwaggerOneOf(
        sum.alts.map { case (name, typ) => name -> typ.map(_.describeWith(name.flatMap(descr.element))) }.toVector,
        cfg.discriminator
      ).describeWith(descr.whole))
}

sealed trait CirceSwaggerInstances {
  implicit def jsonObjectSwagger = SwaggerTypeable.make(SwaggerObject()).as[JsonObject]
}

trait DescribeTypeable[T] {
  def whole: Option[String]
  def element(name: String): Option[SwaggerDescription]
}

object DescribeTypeable {
  def apply[T](implicit instance: DescribeTypeable[T]): DescribeTypeable[T] = instance

  def empty[T]: DescribeTypeable[T] = new DescribeTypeable[T] {
    def whole = None
    def element(name: String) = None
  }

  def make[T](wholeDescr: Option[String], elements: (String, String)*): DescribeTypeable[T] = new DescribeTypeable[T] {
    private[this] val map = elements.toMap
    override def element(name: String) = map.get(name)
    override val whole = wholeDescr
  }
}
