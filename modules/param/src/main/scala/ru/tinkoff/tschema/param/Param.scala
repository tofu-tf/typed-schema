package ru.tinkoff.tschema.param

import java.util.UUID
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.syntax.parallel._
import cats.syntax.traverse._
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import ru.tinkoff.tschema.param.HttpParam.tryParam
import ru.tinkoff.tschema.param.Param.{MultiResult, Result, SingleResult}
import ru.tinkoff.tschema.param.ParamSource.All

import scala.util.control.NonFatal
import scala.util.matching.Regex

sealed trait ParamSource

object ParamSource {
  trait Query          extends ParamSource
  trait Path           extends ParamSource
  trait Header         extends ParamSource
  trait Form           extends ParamSource
  trait MultipartField extends ParamSource
  trait Cookie         extends ParamSource

  trait All extends Query with Path with Header with Form with MultipartField with Cookie

  case object Query          extends Query
  case object Path           extends Path
  case object Header         extends Header
  case object Form           extends Form
  case object MultipartField extends MultipartField
  case object Cookie         extends Cookie
}

sealed trait Param[+S >: All <: ParamSource, A] { self =>
  def map[B](f: A => B): Param[S, B]

  def optional: Param[S, Option[A]]

  def get(name: String, all: String => Option[CharSequence]): Result[A] =
    this match {
      case s: SingleParam[S, A] => s.applyOpt(all(name))
      case m: MultiParam[S, A]  => m.applyOpt(m.names.map(all))
    }
}

trait SingleParam[+S >: All <: ParamSource, A] extends Param[S, A] {
  def applyOpt(source: Option[CharSequence]): SingleResult[A]

  type Self[a] >: HttpSingleParam[a] <: SingleParam[S, a]

  override def map[B](f: A => B): Self[B] = (src => applyOpt(src).map(f)): HttpSingleParam[B]
  override def optional: Self[Option[A]]  = optHttpParam

  protected def optHttpParam: HttpSingleParam[Option[A]] = {
    case None    => Right(None)
    case Some(x) => applyOpt(Some(x)).map(Some(_))
  }
}

object SingleParam extends ParamInstances[SingleParam] {
  def apply[S >: All <: ParamSource, T](implicit param: Param[S, T]) = param
}

trait SingleParamReq[+S >: All <: ParamSource, A] extends SingleParam[S, A] {
  def applyReq(source: CharSequence): SingleResult[A]
  def applyOpt(source: Option[CharSequence]): SingleResult[A] =
    source.fold[SingleResult[A]](Left(MissingParamError))(applyReq)
}

trait MultiParam[+S >: All <: ParamSource, A] extends Param[S, A] { self =>
  def arity: Int = names.length
  def names: List[String]
  def applyOpt(values: List[Option[CharSequence]]): MultiResult[A]

  type Self[a] >: HttpMultiParam[a] <: MultiParam[S, a]

  trait Clone[B] extends HttpMultiParam[B] { def names = self.names }
  override def map[B](f: A => B): Self[B] = (values => applyOpt(values).map(f)): Clone[B]

  def optional: Self[Option[A]] = new HttpMultiParam[Option[A]] {
    def names: List[String]                                                  = self.names
    def applyOpt(values: List[Option[CharSequence]]): MultiResult[Option[A]] =
      self.applyOpt(values) match {
        case Right(a)                 => Right(Some(a))
        case Left(err) if err.missing => Right(None)
        case Left(err)                => Left(err)
      }
  }
}

object MultiParam {
  def apply[A](nameList: List[String], appOpt: List[Option[CharSequence]] => MultiResult[A]): MultiParam[All, A] =
    new MultiParam[All, A] {
      def names: List[String]                                          = nameList
      def applyOpt(values: List[Option[CharSequence]]): MultiResult[A] = appOpt(values)
    }
}

trait MultiParamReq[+S >: All <: ParamSource, A] extends MultiParam[S, A] {
  def applyReq(values: List[CharSequence]): MultiResult[A]
  def applyOpt(values: List[Option[CharSequence]]): MultiResult[A] =
    values
      .zip(names)
      .parTraverse { case (os, name) => Either.fromOption(os, List(name)) }
      .leftMap(names => MultiParamError(names.iterator.map(_ -> MissingParamError).toMap))
      .flatMap(applyReq)
}

sealed trait HttpParam[A] extends Param[All, A] {
  def optional: HttpParam[Option[A]]
}

object HttpParam extends HttpParamInstances[HttpParam] with Derivation[HttpParam] {
  def apply[T](implicit param: HttpParam[T]): HttpParam[T] = param

  trait Enum[E <: enumeratum.EnumEntry] {
    self: enumeratum.Enum[E] =>
    implicit val fromParam: HttpSingleParamReq[E] =
      s => Either.fromOption(withNameOption(s.toString), ParseParamError(s"could not find $self value: $s"))
  }

  def tryParam[T](f: CharSequence => T): HttpSingleParamReq[T] =
    s =>
      try Right(f(s))
      catch {
        case NonFatal(ex) => Left(ParseParamError(ex.toString))
      }

  def empty[A]: HttpMultiParam[List[A]] = new HttpMultiParam[List[A]] {
    def names: List[String]                                                      = Nil
    def applyOpt(values: List[Option[CharSequence]]): MultiResult[List[Nothing]] = Right(Nil)
  }

  def separated[T](sep: Regex)(implicit param: HttpSingleParam[T]): HttpSingleParam[List[T]] =
    s => s.fold(List.empty[T].asRight[SingleParamError])(sep.split(_).toList.traverse(s => param.applyOpt(Some(s))))

  type Typeclass[A] = HttpParam[A]

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = {
    ctx.parameters
      .foldRight[HttpMultiParam[List[Any]]](empty) { (param, prev) =>
        param.typeclass match {
          case single: HttpSingleParam[param.PType] =>
            new HttpMultiParam[List[Any]] {
              def names: List[String]                                                  = param.label +: prev.names
              def applyOpt(values: List[Option[CharSequence]]): MultiResult[List[Any]] = values match {
                case value :: rest =>
                  (single.applyOpt(value).leftMap(_.toMulti(param.label)), prev.applyOpt(rest)).parMapN(_ :: _)
                case Nil           => Left(MultiParamError(Map(param.label -> MissingParamError)))
              }
            }
          case multi: HttpMultiParam[param.PType]   =>
            new HttpMultiParam[List[Any]] {
              override val arity: Int                                                  = multi.arity + prev.arity
              val names: List[String]                                                  = multi.names ++ prev.names
              def applyOpt(values: List[Option[CharSequence]]): MultiResult[List[Any]] =
                (multi.applyOpt(values.take(multi.arity)), prev.applyOpt(values.drop(multi.arity))).parMapN(_ :: _)
            }
        }
      }
      .map(ctx.rawConstruct)
  }

  def generate[T]: Typeclass[T] = macro Magnolia.gen[T]

  def instance[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait HttpSingleParam[A] extends SingleParam[All, A] with HttpParam[A] {
  type Self[a] = HttpSingleParam[a]
}
object HttpSingleParam   extends HttpParamInstances[HttpSingleParam]

trait HttpSingleParamReq[A] extends SingleParamReq[All, A] with HttpSingleParam[A]
trait HttpMultiParam[A]     extends MultiParam[All, A] with HttpParam[A] {
  override type Self[a] = HttpMultiParam[a]
}

object Param extends ParamInstances[Param] {
  type Result[+x]         = Either[ParamError, x]
  type SingleResult[+x]   = Either[SingleParamError, x]
  type MultiResult[+x]    = Either[MultiParamError, x]
  type PQuery[A]          = Param[ParamSource.Query, A]
  type PCookie[A]         = Param[ParamSource.Cookie, A]
  type PForm[A]           = Param[ParamSource.Form, A]
  type PMultipartField[A] = Param[ParamSource.MultipartField, A]
  type PHeader[A]         = Param[ParamSource.Header, A]
  type PPath[A]           = Param[ParamSource.Path, A]
  type PAll[A]            = Param[ParamSource.All, A]

  object PAll

  def apply[S >: All <: ParamSource, T](implicit instance: Param[S, T]): Param[S, T] = instance

  def instance[S >: All <: ParamSource, T](f: Option[CharSequence] => SingleResult[T]): SingleParam[S, T] = param =>
    f(param)

  def separated[S >: All <: ParamSource, T](sep: Regex)(implicit param: SingleParam[S, T]): SingleParam[S, List[T]] =
    s => s.fold(List.empty[T].asRight[SingleParamError])(sep.split(_).toList.traverse(s => param.applyOpt(Some(s))))

  def empty[A]: MultiParam[All, List[A]] = new MultiParam[All, List[A]] {
    def names: List[String]                                                      = Nil
    def applyOpt(values: List[Option[CharSequence]]): MultiResult[List[Nothing]] = Right(Nil)
  }

  trait Enum[S >: All <: ParamSource, E <: enumeratum.EnumEntry] {
    self: enumeratum.Enum[E] =>
    implicit val fromParam: SingleParamReq[S, E] =
      s => Either.fromOption(withNameOption(s.toString), ParseParamError(s"could not find $self value: $s"))
  }

}

trait LowPriorParamInstances[P[s >: All <: ParamSource, a] >: SingleParam[s, a]] {
  implicit def optionalParam[S >: All <: ParamSource, A](implicit param: Param[S, A]): Param[S, Option[A]] =
    param.optional
}

trait ParamInstances[P[s >: All <: ParamSource, a] >: SingleParam[s, a]]
    extends LowPriorParamInstances[P] with PrimitiveParamInstances[P[All, *]] {

  implicit def optSingleParam[S >: All <: ParamSource, A](implicit
      param: SingleParam[S, A]
  ): SingleParam[S, Option[A]] =
    param.optional
  implicit def optMultiParam[S >: All <: ParamSource, A](implicit param: MultiParam[S, A]): MultiParam[S, Option[A]] =
    param.optional
}

trait LowPriorHttpParamInstances[P[a] >: HttpSingleParam[a]] {
  implicit def optionalParam[A](implicit param: HttpParam[A]): HttpParam[Option[A]] = param.optional
}

trait HttpParamInstances[P[a] >: HttpSingleParam[a]]
    extends LowPriorHttpParamInstances[P] with PrimitiveParamInstances[P] {
  implicit def optSingleParam[A](implicit param: HttpSingleParam[A]): HttpSingleParam[Option[A]] =
    param.optional
  implicit def optMultiParam[A](implicit param: HttpMultiParam[A]): HttpMultiParam[Option[A]]    =
    param.optional
}

trait PrimitiveParamInstances[P[a] >: HttpSingleParam[a]] {
  implicit val stringParam: P[String]         = tryParam(_.toString)
  implicit val intParam: P[Int]               = tryParam(_.toString.toInt)
  implicit val doubleParam: P[Double]         = tryParam(_.toString.toDouble)
  implicit val floatParam: P[Float]           = tryParam(_.toString.toFloat)
  implicit val longParam: P[Long]             = tryParam(_.toString.toLong)
  implicit val bigIntParam: P[BigInt]         = tryParam(s => BigInt(s.toString))
  implicit val booleanParam: P[Boolean]       = tryParam(_.toString.toBoolean)
  implicit val bigDecimalParam: P[BigDecimal] = tryParam(s => BigDecimal(s.toString))
  implicit val uuidParam: P[UUID]             = tryParam(s => UUID.fromString(s.toString))
}

sealed trait ParamError
sealed trait SingleParamError                                         extends ParamError {
  def toMulti(name: String): MultiParamError = MultiParamError(Map(name -> this))
}
case object MissingParamError                                         extends SingleParamError
final case class ParseParamError(message: String)                     extends SingleParamError
final case class MultiParamError(list: Map[String, SingleParamError]) extends ParamError {
  def missing: Boolean = list.valuesIterator.forall { _ == MissingParamError }
}

object MultiParamError {
  implicit val semigroup: Semigroup[MultiParamError] = (e1, e2) => MultiParamError(e1.list ++ e2.list)
}
