package ru.tinkoff.travel.schema.serve

import akka.http.scaladsl.server._
import Directives._
import akka.http.scaladsl.model.HttpHeader
import ru.tinkoff.travel.schema.serve.ListParamOptions.default

import scala.language.higherKinds

trait FromParam[T] {
  type Self[x] <: FromParam[x]

  def apply(param: String): T

  def map[X](f: T ⇒ X): Self[X]
}
object FromParam{
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

trait FromQueryParam[T] extends FromParam[T] {
  self ⇒
  type Self[X] = FromQueryParam[X]

  def apply(param: String): T

  override def map[S](f: T ⇒ S) = new FromQueryParam[S] {
    def apply(param: String): S = f(self(param))
  }
}

trait LowPriorityFromParamCompanion[F[x] <: FromParam.Aux[x, F]] {
  self: FromParamCompanion[F] ⇒
  implicit def listParam[X](implicit fromParam: F[X], options: ListParamOptions[F] = default[F]): F[List[X]] =
    param {
      case "" ⇒ List.empty[X]
      case str ⇒ str.split(options.sep).iterator.map(x ⇒ fromParam(x): X).toList
    }
}

trait FromParamCompanion[F[x] <: FromParam.Aux[x, F]]  extends LowPriorityFromParamCompanion[F]{
  def param[T](f: String ⇒ T): F[T]

  implicit val stringParam = param(identity)
  implicit val intParam = param(_.toInt)
  implicit val doubleParam = param(_.toDouble)
  implicit val floatParam = param(_.toFloat)
  implicit val longParam = param(_.toLong)
  implicit val bigIntParam = param(BigInt.apply)
  implicit val bigDecimalParam = param(BigDecimal.apply)
  implicit val booleanParam = param(_.toBoolean)

  implicit def list2Param[X](implicit fromParam: F[List[X]], options: ListParamOptions[F] = default[F]): F[List[List[X]]] =
    param(_.split(options.sep2).view.map(fromParam.apply).toList)
}

object FromQueryParam extends FromParamCompanion[FromQueryParam] {
  def param[T](f: String ⇒ T) = new FromQueryParam[T] {
    def apply(param: String): T = f(param)
  }
}

class FromPathParam[T](val matcher: PathMatcher1[T])

object FromPathParam {
  implicit object stringParam extends FromPathParam(Segment)
  implicit object intParam extends FromPathParam(IntNumber)
  implicit object longParam extends FromPathParam(LongNumber)
  implicit object doubleParam extends FromPathParam(DoubleNumber)
  implicit object uuidParam extends FromPathParam(JavaUUID)
}

trait FromHeader[T] extends FromParam[T]{
  self ⇒
  type Self[x] = FromHeader[x]

  def apply(header: HttpHeader): Option[T]
  def apply(name: String): T

  def map[X](f: (T) ⇒ X): Self[X] = new FromHeader[X] {
    def apply(header: HttpHeader)  = self(header).map(f)
    def apply(name: String) = f(self(name))
}
}

trait FromFormField[T] extends FromParam[T] {
  self ⇒
  type Self[x] = FromFormField[x]

  def apply(param: String): T

  override def map[X](f: (T) ⇒ X) = new FromFormField[X] {
    def apply(param: String): X = f(self(param))
  }
}

object FromFormField extends FromParamCompanion[FromFormField] {
  def param[T](f: String ⇒ T) = new FromFormField[T] {
    def apply(param: String): T = f(param)
  }
}

trait FromCookie[T] extends FromParam[T] {
  self ⇒
  type Self[x] = FromCookie[x]

  def apply(param: String): T

  override def map[X](f: (T) ⇒ X) = new FromCookie[X] {
    def apply(param: String): X = f(self(param))
  }
}

object FromCookie extends FromParamCompanion[FromCookie] {
  def param[T](f: String ⇒ T) = new FromCookie[T] {
    def apply(param: String): T = f(param)
  }
}
