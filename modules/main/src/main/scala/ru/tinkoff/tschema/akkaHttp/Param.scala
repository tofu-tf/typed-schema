package ru.tinkoff.tschema.akkaHttp

import java.util.UUID

import Param.{MultiResult, Result, SingleResult, tryParam}
import ParamSource.All
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.instances.parallel._
import cats.instances.map._
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.syntax.parallel._
import cats.syntax.traverse._
import cats.syntax.parallel._
import magnolia.{CaseClass, Magnolia, SealedTrait}

import scala.language.higherKinds
import scala.util.control.NonFatal

sealed trait ParamSource

object ParamSource {
  trait Query  extends ParamSource
  trait Path   extends ParamSource
  trait Header extends ParamSource
  trait Form   extends ParamSource
  trait Cookie extends ParamSource

  trait All extends Query with Path with Header with Form with Cookie
}

sealed trait Param[+S <: ParamSource, +A] { self =>
  def map[B](f: A => B): Param[S, B]

  def optional: Param[S, Option[A]]

  def get(name: String, all: String => Option[String]): Result[A] =
    this match {
      case s: SingleParam[S, A] => s.applyOpt(all(name))
      case m: MultiParam[S, A]  => m.applyOpt(m.names.map(all))
    }
}

trait SingleParam[+S <: ParamSource, +A] extends Param[S, A] {
  def applyOpt(source: Option[String]): SingleResult[A]
  override def map[B](f: A => B): SingleParam[S, B] = src => applyOpt(src).map(f)
  override def optional: SingleParam[S, Option[A]] = {
    case None    => Right(None)
    case Some(x) => applyOpt(Some(x)).map(Some(_))
  }
}

object SingleParam extends ParamInstances[SingleParam]

trait SingleParamReq[+S <: ParamSource, A] extends SingleParam[S, A] {
  def applyReq(source: String): SingleResult[A]
  def applyOpt(source: Option[String]): SingleResult[A] = source.fold[SingleResult[A]](Left(MissingParamError))(applyReq)
}

trait MultiParam[+S <: ParamSource, +A] extends Param[S, A] { self =>
  def arity: Int = names.length
  def names: List[String]
  def applyOpt(values: List[Option[String]]): MultiResult[A]

  trait Clone[B] extends MultiParam[S, B] { def names = self.names }
  override def map[B](f: A => B): Clone[B] = values => applyOpt(values).map(f)
  override def optional: MultiParam[S, Option[A]] = new MultiParam[S, Option[A]] {
    def names: List[String] = self.names
    def applyOpt(values: List[Option[String]]): MultiResult[Option[A]] =
      values.sequence.traverse(vals => self.applyOpt(vals.map(Some(_))))
  }
}

object MultiParam {
  def apply[A](nameList: List[String], appOpt: List[Option[String]] => MultiResult[A]): MultiParam[All, A] =
    new MultiParam[All, A] {
      def names: List[String]                                    = nameList
      def applyOpt(values: List[Option[String]]): MultiResult[A] = appOpt(values)
    }
}

trait MultiParamReq[S <: ParamSource, A] extends MultiParam[S, A] {
  def applyReq(values: List[String]): MultiResult[A]
  def applyOpt(values: List[Option[String]]): MultiResult[A] =
    values
      .zip(names)
      .parTraverse { case (os, name) => Either.fromOption(os, List(name)) }
      .leftMap(names => MultiParamError(names.iterator.map(_ -> MissingParamError).toMap))
      .flatMap(applyReq)
}

object Param extends ParamInstances[Param] {
  type Result[+x]       = Either[ParamError, x]
  type SingleResult[+x] = Either[SingleParamError, x]
  type MultiResult[+x]  = Either[MultiParamError, x]
  type PQuery[A]        = Param[ParamSource.Query, A]
  type PCookie[A]       = Param[ParamSource.Cookie, A]
  type PForm[A]         = Param[ParamSource.Form, A]
  type PHeader[A]       = Param[ParamSource.Header, A]
  type PPath[A]         = Param[ParamSource.Path, A]
  type PAll[A]          = Param[ParamSource.All, A]

  def apply[S <: ParamSource, T](implicit instance: Param[S, T]): Param[S, T] = instance

  def getOrReport(dict: String => Option[String])(name: String): Either[List[String], String] =
    Either.fromOption(dict(name), List(name))

  def tryParam[S <: ParamSource, T](f: String => T): SingleParamReq[S, T] =
    s =>
      try Right(f(s))
      catch {
        case NonFatal(ex) => Left(ParseParamError(ex.toString))
    }

  def instance[S <: ParamSource, T](f: Option[String] => SingleResult[T]): SingleParam[S, T] = param => f(param)

  def separated[S <: ParamSource, T](sep: String)(implicit param: SingleParam[S, T]): SingleParam[S, List[T]] =
    s => s.fold(List.empty[T].asRight[SingleParamError])(_.split(sep).toList.traverse(s => param.applyOpt(Some(s))))

  val empty: MultiParam[All, List[Nothing]] = new MultiParam[All, List[Nothing]] {
    def names: List[String]                                                = Nil
    def applyOpt(values: List[Option[String]]): MultiResult[List[Nothing]] = Right(Nil)
  }

  trait Enum[S <: ParamSource, E <: enumeratum.EnumEntry] {
    self: enumeratum.Enum[E] =>
    implicit val fromParam: SingleParamReq[S, E] =
      s => Either.fromOption(withNameOption(s), ParseParamError(s"could not find $self value: $s"))
  }

  type Typeclass[A] = Param[All, A]

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = {
    ctx.parameters
      .foldRight[MultiParam[All, List[Any]]](empty) { (param, prev) =>
        param.typeclass match {
          case single: SingleParam[All, param.PType] =>
            new MultiParam[All, List[Any]] {
              def names: List[String] = param.label +: prev.names
              def applyOpt(values: List[Option[String]]): MultiResult[List[Any]] = values match {
                case value :: rest =>
                  (single.applyOpt(value).leftMap(_.toMulti(param.label)), prev.applyOpt(rest)).parMapN(_ :: _)
                case Nil => Left(MultiParamError(Map(param.label -> MissingParamError)))
              }
            }
          case multi: MultiParam[All, param.PType] =>
            new MultiParam[All, List[Any]] {
              override val arity: Int = multi.arity + prev.arity
              val names: List[String] = multi.names ++ prev.names
              def applyOpt(values: List[Option[String]]): MultiResult[List[Any]] =
                (multi.applyOpt(values.take(multi.arity)), prev.applyOpt(values.drop(multi.arity))).parMapN(_ :: _)
            }
        }
      }
      .map(ctx.rawConstruct)
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  def generate[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait LowPriorParamInstances[P[s <: ParamSource, a] >: SingleParam[s, a]] {
  implicit def optionalParam[S <: ParamSource, A](implicit param: Param[S, A]): Param[S, Option[A]] = param.optional
}

trait ParamInstances[P[s <: ParamSource, a] >: SingleParam[s, a]] extends LowPriorParamInstances[P] {
  implicit val stringParam: P[All, String]         = tryParam(identity)
  implicit val intParam: P[All, Int]               = tryParam(_.toInt)
  implicit val doubleParam: P[All, Double]         = tryParam(_.toDouble)
  implicit val floatParam: P[All, Float]           = tryParam(_.toFloat)
  implicit val longParam: P[All, Long]             = tryParam(_.toLong)
  implicit val bigIntParam: P[All, BigInt]         = tryParam(BigInt.apply)
  implicit val booleanParam: P[All, Boolean]       = tryParam(_.toBoolean)
  implicit val bigDecimalParam: P[All, BigDecimal] = tryParam(BigDecimal.apply)
  implicit val uuidParam: P[All, UUID]             = tryParam(UUID.fromString)

  implicit def optSingleParam[S <: ParamSource, A](implicit param: SingleParam[S, A]): SingleParam[S, Option[A]] =
    param.optional
  implicit def optMultiParam[S <: ParamSource, A](implicit param: MultiParam[S, A]): MultiParam[S, Option[A]] = param.optional
}

sealed trait ParamError
sealed trait SingleParamError extends ParamError {
  def toMulti(name: String): MultiParamError = MultiParamError(Map(name -> this))
}
case object MissingParamError                                         extends SingleParamError
final case class ParseParamError(message: String)                     extends SingleParamError
final case class MultiParamError(list: Map[String, SingleParamError]) extends ParamError

object MultiParamError {
  implicit val semigroup: Semigroup[MultiParamError] = (e1, e2) => MultiParamError(e1.list ++ e2.list)
}
