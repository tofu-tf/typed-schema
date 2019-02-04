package ru.tinkoff.tschema.akkaHttp

import java.util.UUID

import Param.{Fold, Result}
import Source.All
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import cats.Monad
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.instances.either._
import cats.instances.parallel._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.syntax.parallel._

import scala.language.higherKinds
import scala.util.control.NonFatal

sealed trait Source

object Source {
  trait Query  extends Source
  trait Path   extends Source
  trait Header extends Source
  trait Form   extends Source
  trait Cookie extends Source

  trait All extends Query with Path with Header with Form with Cookie
}

trait Param[+S <: Source, A] { self =>
  def fold[R](fld: Fold[A, R]): R

  def map[B](f: A => B): Param[S, B] = new Param[S, B] {
    def fold[R](fld: Fold[B, R]): R = self.fold(fld.contramap(f))
  }

  def get(name: String, all: String => Option[String]): Result[A] =
    fold(new Fold[A, Result[A]] {
      def single(value: Option[String] => Result[A])                                       = value(all(name))
      def multi(names: List[String], values: List[Option[String]] => Result[A]): Result[A] = values(names.map(all))
    })

  def optional: Param[S, Option[A]] =
    fold(new Fold[A, Param[S, Option[A]]] {
      def single(value: Option[String] => Result[A]): SingleParam[S, Option[A]] = ???

      def multi(names: List[String], values: List[Option[String]] => Result[A]): Param[S, Option[A]] = ???
    })
}

trait SingleParam[+S <: Source, A] extends Param[S, A] {
  def applyOpt(source: Option[String]): Result[A]
  def fold[R](fld: Fold[A, R]): R                   = fld.single(applyOpt)
  override def map[B](f: A => B): SingleParam[S, B] = src => applyOpt(src).map(f)
}

trait SingleParamReq[+S <: Source, A] extends SingleParam[S, A] {
  def applyReq(source: String): Result[A]
  def applyOpt(source: Option[String]): Result[A] = source.fold[Result[A]](Left("value missing"))(applyReq)
}

trait MultiParam[S <: Source, A] extends Param[S, A] { self =>
  def names: List[String]
  def applyOpt(values: List[Option[String]]): Result[A]

  def fold[R](fld: Fold[A, R]): R = fld.multi(names, applyOpt)
  trait Clone[B] extends MultiParam[S, B] { def names = self.names }
  override def map[B](f: A => B): Clone[B] = values => applyOpt(values).map(f)
}

trait MultiParamReq[S <: Source, A] extends MultiParam[S, A] {
  def applyReq(values: List[String]): Result[A]
  def applyOpt(values: List[Option[String]]): Result[A] =
    values
      .zip(names)
      .parTraverse { case (os, name) => Either.fromOption(os, List(name)) }
      .leftMap(names => s"missing fields ${names.mkString(", ")}")
      .flatMap(applyReq)
}

object Param {
  type Result[x] = Either[String, x]
  type Query[A]  = Param[Source.Query, A]
  type Cookie[A] = Param[Source.Cookie, A]
  type Form[A]   = Param[Source.Form, A]
  type Header[A] = Param[Source.Header, A]
  type Path[A]   = Param[Source.Path, A]

  trait Fold[A, R] { self =>
    def single(value: Option[String] => Result[A]): R
    def multi(names: List[String], values: List[Option[String]] => Result[A]): R

    def contramap[B](f: B => A): Fold[B, R] = new Fold[B, R] {
      def single(value: Option[String] => Result[B]): R =
        self.single(s => value(s).map(f))
      def multi(names: List[String], values: List[Option[String]] => Result[B]): R =
        self.multi(names, l => values(l).map(f))
    }
  }

  def apply[S <: Source, T](implicit instance: Param[S, T]): Param[S, T] = instance

  def getOrReport(dict: String => Option[String])(name: String): Either[List[String], String] =
    Either.fromOption(dict(name), List(name))

  def tryParam[S <: Source, T](f: String => T): SingleParamReq[S, T] =
    s =>
      try Right(f(s))
      catch {
        case NonFatal(ex) => Left(ex.toString)
    }

  def instance[S <: Source, T](f: Option[String] => Result[T]): SingleParam[S, T] = param => f(param)

  def separated[S <: Source, T](sep: String)(implicit param: SingleParam[S, T]): SingleParam[S, List[T]] =
    s => s.fold(List.empty[T].asRight[String])(_.split(sep).toList.traverse(s => param.applyOpt(Some(s))))

  implicit val stringParam: SingleParam[All, String]         = tryParam(identity)
  implicit val intParam: SingleParam[All, Int]               = tryParam(_.toInt)
  implicit val doubleParam: SingleParam[All, Double]         = tryParam(_.toDouble)
  implicit val floatParam: SingleParam[All, Float]           = tryParam(_.toFloat)
  implicit val longParam: SingleParam[All, Long]             = tryParam(_.toLong)
  implicit val bigIntParam: SingleParam[All, BigInt]         = tryParam(BigInt.apply)
  implicit val booleanParam: SingleParam[All, Boolean]       = tryParam(_.toBoolean)
  implicit val bigDecimalParam: SingleParam[All, BigDecimal] = tryParam(BigDecimal.apply)
  implicit val uuidParam: SingleParam[All, UUID]             = tryParam(UUID.fromString)

  def rejectionHandler: PartialFunction[Rejection, Route] = {
    case ParamFormatRejection(name, error) => complete(BadRequest, s"could not parse parameter $name : $error")
    case ParamRecordRejection(errors) =>
      val parStr = errors.map { case (name, error) => s"$name : $error" }.mkString("\n")
      complete(BadRequest, s"could not parse parameters:\n $parStr")
  }

  trait Enum[S <: Source, E <: enumeratum.EnumEntry] {
    self: enumeratum.Enum[E] =>
    implicit val fromParam: SingleParamReq[S, E] =
      s => Either.fromOption(withNameOption(s), s"could not find $self value: $s")
  }
}

final case class ParamFormatRejection(name: String, error: String)    extends Rejection
final case class ParamRecordRejection(errors: List[(String, String)]) extends Rejection
