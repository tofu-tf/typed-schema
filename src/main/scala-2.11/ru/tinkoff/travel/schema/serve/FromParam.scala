package ru.tinkoff.tschema.serve

import akka.http.scaladsl.server._
import Directives._
import akka.http.scaladsl.model.HttpHeader

import scala.language.higherKinds

trait FromParam[T] {
  type Self[x] <: FromParam[x]

  def apply(param: String): T

  def map[X](f: T ⇒ X): Self[X]
}

trait FromQueryParam[T] extends FromParam[T] {
  self ⇒
  type Self[X] = FromQueryParam[X]

  def apply(param: String): T

  override def map[S](f: T ⇒ S) = new FromQueryParam[S] {
    def apply(param: String): S = f(self(param))
  }
}

trait LowPriorityFromParamCompanion[F[x] <: FromParam[x] {type Self[y] = F[y]}] {
  self: FromParamCompanion[F] ⇒
  implicit def listParam[X](implicit fromParam: F[X]): F[List[X]] =
    param(_.split(',').view.map(fromParam.apply).toList)
}

trait FromParamCompanion[F[x] <: FromParam[x] {type Self[y] = F[y]}] {
  def param[T](f: String ⇒ T): F[T]

  implicit val stringParam = param(identity)
  implicit val intParam = param(_.toInt)
  implicit val doubleParam = param(_.toDouble)
  implicit val floatParam = param(_.toFloat)
  implicit val longParam = param(_.toLong)
  implicit val bigIntParam = param(BigInt.apply)
  implicit val bigDecimalParam = param(BigDecimal.apply)

  implicit def list2Param[X](implicit fromParam: F[List[X]]): F[List[List[X]]] =
    param(_.split(';').view.map(fromParam.apply).toList)
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

trait FromHeader[T] {
  def apply(header: HttpHeader): Option[T]
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
