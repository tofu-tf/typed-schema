package ru.tinkoff.travel.schema.serve

import akka.http.scaladsl.server._
import Directives._
import akka.http.scaladsl.model.HttpHeader
import ru.tinkoff.travel.schema.serve.ListParamOptions.default
import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Witness}

import scala.language.higherKinds

trait FromParam[T] {
  type Self[x] <: FromParam[x]

  def apply(param: String): T

  def map[X](f: T ⇒ X): Self[X]
}

object FromParam {
  type Aux[T, F[x] <: FromParam[x]] = FromParam[T] {type Self[x] = F[x]}
}
/**
  * Options for parsing collections as parameters
  */
trait ListParamOptions[F[x] <: FromParam[x]] {
  /**
    * separator for parsing lists of primitives
    */
  def sep: Char
  /**
    * separator for parsing lists of lists of primitives
    */
  def sep2: Char
}

object ListParamOptions {
  def apply[F[x] <: FromParam[x]](separator: Char, separator2: Char) = new ListParamOptions[F] {
    def sep: Char = separator
    def sep2: Char = separator2
  }

  def default[F[x] <: FromParam[x]]: ListParamOptions[F] = apply[F](',', ';')
}

trait LowPriorityFromParamCompanion[F[x] <: FromParam.Aux[x, F]] {
  self: FromParamCompanion[F] ⇒

  implicit def listParam[X](implicit fromParam: F[X], options: ListParamOptions[F] = default[F]) =
    param {
      case "" ⇒ List.empty[X]
      case str ⇒ str.split(options.sep).iterator.map(x ⇒ fromParam(x): X).toList
    }
}

trait FromParamCompanion[F[x] <: FromParam.Aux[x, F]] extends LowPriorityFromParamCompanion[F] {
  def param[T](f: String ⇒ T): F[T]

  implicit val stringParam = param(identity)
  implicit val intParam = param(_.toInt)
  implicit val doubleParam = param(_.toDouble)
  implicit val floatParam = param(_.toFloat)
  implicit val longParam = param(_.toLong)
  implicit val bigIntParam = param(BigInt.apply)
  implicit val booleanParam = param(_.toBoolean)
  implicit val bigDecimalParam = param(BigDecimal.apply)

  implicit def list2Param[X](implicit fromParam: F[List[X]], options: ListParamOptions[F] = default[F]): F[List[List[X]]] =
    param(_.split(options.sep2).view.map(fromParam.apply).toList)
}

trait FromQueryParam[T] extends FromParam[T] {
  self ⇒
  type Self[X] = FromQueryParam[X]

  def apply(param: String): T

  override def map[S](f: T ⇒ S): FromQueryParam[S] = (s: String) ⇒ f(self(s))
}

object FromQueryParam extends FromParamCompanion[FromQueryParam] {
  def param[T](f: String ⇒ T): FromQueryParam[T] = s ⇒ f(s)
}

class FromPathParam[T](val matcher: PathMatcher1[T])

object FromPathParam {
  implicit object stringParam extends FromPathParam(Segment)
  implicit object intParam extends FromPathParam(IntNumber)
  implicit object longParam extends FromPathParam(LongNumber)
  implicit object doubleParam extends FromPathParam(DoubleNumber)
  implicit object uuidParam extends FromPathParam(JavaUUID)
}

trait FromHeader[T] extends FromParam[T] {
  self ⇒

  def apply(header: HttpHeader): Option[T]
  def apply(name: String): T

  type Self[x] = FromHeader[x]
  def map[X](f: (T) ⇒ X): FromHeader[X] = new FromHeader[X] {
    def apply(header: HttpHeader): Option[X] = self(header).map(f)
    def apply(name: String): X = f(self(name))
  }
}

trait FromFormField[T] extends FromParam[T] {
  self ⇒
  type Self[x] = FromFormField[x]

  def apply(param: String): T

  override def map[X](f: (T) ⇒ X) = s ⇒ f(self(s))
}

object FromFormField extends FromParamCompanion[FromFormField] {
  def param[T](f: String ⇒ T): FromFormField[T] = s ⇒ f(s)
}

trait FromCookie[T] extends FromParam[T] {
  self ⇒
  type Self[x] = FromCookie[x]

  def apply(param: String): T

  override def map[X](f: (T) ⇒ X) = (s) ⇒ f(self(s))
}

object FromCookie extends FromParamCompanion[FromCookie] {
  def param[T](f: String ⇒ T): FromCookie[T] = s ⇒ f(s)
}

trait ParamRecord[F[x] <: FromParam[x], C] {
  def apply(params: Map[String, String]): Either[List[String], C]
}

object ParamRecord {
  implicit def hnilRecord[F[x] <: FromParam[x]]: ParamRecord[F, HNil] = _ ⇒ Right(HNil)

  private def concatErrors[S, A, T <: HList](head: Either[String, A], tail: Either[List[String], T]): Either[List[String], A :: T] =
    (head, tail) match {
      case (Right(h), Right(t)) ⇒ Right(h :: t)
      case (Right(h), Left(names)) ⇒ Left(names)
      case (Left(name), Right(_)) ⇒ Left(List(name))
      case (Left(name), Left(names)) ⇒ Left(name :: names)
    }

  implicit def hconsRecord[F[x] <: FromParam.Aux[x, F], S <: Symbol, H, Tail <: HList]
  (implicit head: F[H], tail: ParamRecord[F, Tail], S: Witness.Aux[S]): ParamRecord[F, FieldType[S, H] :: Tail] =
    map ⇒ {
      val name = S.value.name
      val tr = tail(map)
      val hr = map.get(name).map(s ⇒ field[S](head(s))).toRight(name)
      concatErrors(hr, tr)
    }

  implicit def hconsOptRecord[F[x] <: FromParam.Aux[x, F], S <: Symbol, H, Tail <: HList]
  (implicit head: F[H], tail: ParamRecord[F, Tail], S: Witness.Aux[S]): ParamRecord[F, FieldType[S, Option[H]] :: Tail] =
    map ⇒ {
      val name = S.value.name
      val tr = tail(map)
      val hr = field[S](map.get(name).map(s ⇒ head(s)))
      tr.map(hr :: _)
    }

  implicit def generic[F[x] <: FromParam.Aux[x, F], C, L <: HList]
  (implicit lgen: LabelledGeneric.Aux[C, L], record: ParamRecord[F, L]): ParamRecord[F, C] =
    map ⇒ record(map).map(lgen.from)
}


