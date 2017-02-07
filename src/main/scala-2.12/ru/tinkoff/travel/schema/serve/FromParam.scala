package ru.tinkoff.travel.schema.serve

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

  override def map[S](f: T ⇒ S): FromQueryParam[S] = (s: String) ⇒ f(self(s))
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

trait FromHeader[T] {
  def apply(header: HttpHeader): Option[T]
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
