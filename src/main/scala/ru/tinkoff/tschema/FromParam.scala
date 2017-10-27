package ru.tinkoff.tschema

import java.util.UUID

import akka.http.scaladsl.server._
import Directives._
import FromParam.Result
import akka.http.scaladsl.model.HttpHeader
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._
import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Witness}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes._

import scala.language.higherKinds
import scala.util.control.NonFatal
import ListParamOptions._
import cats.data.NonEmptyList
import ru.tinkoff.tschema.ParamRecord.NelRes

trait FromParam[T] {
  type Self[x] <: FromParam[x]

  def apply(param: String): Result[T]

  def map[X](f: T => X): Self[X]
}

object FromParam {
  type Aux[T, F[x] <: FromParam[x]] = FromParam[T] {type Self[x] = F[x]}
  type Result[x] = Either[String, x]

  def rejectionHandler: PartialFunction[Rejection, Route] = {
    case ParamFormatRejection(name, error) => complete(BadRequest, s"could not parse parameter $name : $error")
    case ParamRecordRejection(errors)      =>
      val parStr = errors.map { case (name, error) => s"$name : $error" }.mkString("\n")
      complete(BadRequest, s"could not parse parameters:\n $parStr")
  }

  trait Enum[E <: enumeratum.EnumEntry] {
    self: enumeratum.Enum[E] =>
    private def fromParam[C <: FromParamFactory](comp: C): comp.TC[E] =
      comp.param(s => self.withNameOption(s).fold[Either[String, E]](Left(s"could not find $self value: $s"))(x => Right(x)))


    implicit lazy val fromQueryParam: FromQueryParam[E] = fromParam(FromQueryParam)
    implicit lazy val fromHeader: FromHeader[E] = fromParam(FromHeader)
    implicit lazy val fromFormField: FromFormField[E] = fromParam(FromFormField)
    implicit lazy val fromPathParam = new FromPathParam[E](Segment.flatMap(self.withNameOption))
  }
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
  self: FromParamCompanion[F] =>

  implicit def listParam[X](implicit fromParam: F[X], options: ListParamOptions[F] = default[F]) =
    param {
      case ""  => Right(List.empty[X])
      case str => str.split(options.sep).toList.traverse[Result, X](fromParam.apply)
    }
}

trait FromParamFactory {
  type TC[x] <: FromParam.Aux[x, TC]
  def param[T](f: String => Result[T]): TC[T]
  def tryParam[T](f: String => T): TC[T] = param(s => try Right(f(s)) catch {case NonFatal(ex) => Left(ex.toString)})
}

trait FromParamCompanion[F[x] <: FromParam.Aux[x, F]] extends LowPriorityFromParamCompanion[F] with FromParamFactory {

  type TC[x] = F[x]

  implicit val stringParam = tryParam(identity)
  implicit val intParam = tryParam(_.toInt)
  implicit val doubleParam = tryParam(_.toDouble)
  implicit val floatParam = tryParam(_.toFloat)
  implicit val longParam = tryParam(_.toLong)
  implicit val bigIntParam = tryParam(BigInt.apply)
  implicit val booleanParam = tryParam(_.toBoolean)
  implicit val bigDecimalParam = tryParam(BigDecimal.apply)
  implicit val uuidParam = tryParam(UUID.fromString)

  implicit def list2Param[X](implicit fromParam: F[List[X]], options: ListParamOptions[F] = default[F]): F[List[List[X]]] =
    param(_.split(options.sep2).toList.traverse[Result, List[X]](fromParam.apply))
}

trait FromQueryParam[T] extends FromParam[T] {
  self =>
  type Self[X] = FromQueryParam[X]

  def apply(param: String): Result[T]

  override def map[S](f: T => S): FromQueryParam[S] = new FromQueryParam[S] {
    def apply(param: String): Result[S] = self(param).map(f)
  }
}

object FromQueryParam extends FromParamCompanion[FromQueryParam] {
  def param[T](f: String => Result[T]): FromQueryParam[T] = new FromQueryParam[T] {
    def apply(param: String): Result[T] = f(param)
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

trait FromHeader[T] extends FromParam[T] {
  self =>

  def apply(name: String): Result[T]

  type Self[x] = FromHeader[x]
  def map[X](f: (T) => X): FromHeader[X] = new FromHeader[X] {
    def apply(name: String): Result[X] = self(name).map(f)
  }
}
object FromHeader extends FromParamCompanion[FromHeader] {
  def param[T](f: (String) => Result[T]) = new FromHeader[T] {
    def apply(param: String): Result[T] = f(param)
  }
}

trait FromFormField[T] extends FromParam[T] {
  self =>
  type Self[x] = FromFormField[x]

  def apply(param: String): Result[T]

  override def map[X](f: (T) => X) = new FromFormField[X] {
    def apply(param: String): Result[X] = self(param).map(f)
  }
}

object FromFormField extends FromParamCompanion[FromFormField] {
  def param[T](f: String => Result[T]): FromFormField[T] = new FromFormField[T] {
    def apply(param: String): Result[T] = f(param)
  }
}

trait FromCookie[T] extends FromParam[T] {
  self =>
  type Self[x] = FromCookie[x]

  def apply(param: String): Result[T]

  override def map[X](f: (T) => X) = new FromCookie[X] {
    def apply(param: String): Result[X] = self(param).map(f)
  }
}

object FromCookie extends FromParamCompanion[FromCookie] {
  def param[T](f: String => Result[T]): FromCookie[T] = new FromCookie[T] {
    def apply(param: String): Result[T] = f(param)
  }
}

trait ParamRecord[F[x] <: FromParam[x], C] {
  def apply(params: String => Option[String]): NelRes[C]
}

object ParamRecord {
  type NelRes[X] = Either[NonEmptyList[(String, String)], X]

  val notFound = "Not Found"

  implicit def hnilRecord[F[x] <: FromParam[x]]: ParamRecord[F, HNil] =
    new ParamRecord[F, HNil] {
      def apply(params: String => Option[String]): NelRes[HNil] = Right(HNil)
    }

  private def concatErrors[S, A, T <: HList](name: String, head: Either[String, A], tail: NelRes[T]): NelRes[A :: T] =
    (head, tail) match {
      case (Right(h), Right(t))     => Right(h :: t)
      case (Right(h), Left(errs))   => Left(errs)
      case (Left(err), Right(_))    => Left(NonEmptyList.of(name -> err))
      case (Left(err), Left(names)) => Left((name -> err) :: names)
    }

  implicit def hconsRecord[F[x] <: FromParam.Aux[x, F], S <: Symbol, H, Tail <: HList]
  (implicit head: F[H], tail: ParamRecord[F, Tail], S: Witness.Aux[S]): ParamRecord[F, FieldType[S, H] :: Tail] =
    new ParamRecord[F, FieldType[S, H] :: Tail] {
      def apply(params: String => Option[String]): NelRes[FieldType[S, H] :: Tail] = {
        val name = S.value.name
        val tr = tail(params)
        val hr = params(name).toRight(notFound).flatMap(s => head(s).map(field[S].apply))
        concatErrors(name, hr, tr)
      }
    }

  implicit def hconsOptRecord[F[x] <: FromParam.Aux[x, F], S <: Symbol, H, Tail <: HList]
  (implicit head: F[H], tail: ParamRecord[F, Tail], S: Witness.Aux[S]): ParamRecord[F, FieldType[S, Option[H]] :: Tail] =
    new ParamRecord[F, FieldType[S, Option[H]] :: Tail] {
      def apply(params: String => Option[String]): NelRes[FieldType[S, Option[H]] :: Tail] = {
        val name = S.value.name
        val tr = tail(params)
        val hr = field[S](params(name).flatMap(s => head(s).toOption))
        tr.map(hr :: _)
      }
    }

  implicit def generic[F[x] <: FromParam.Aux[x, F], C, L <: HList]
  (implicit lgen: LabelledGeneric.Aux[C, L], record: ParamRecord[F, L]): ParamRecord[F, C] =
    new ParamRecord[F, C] {
      def apply(map: String => Option[String]): NelRes[C] = record(map).map(lgen.from)
    }
}

final case class ParamFormatRejection(name: String, error: String) extends Rejection
final case class ParamRecordRejection(errors: List[(String, String)]) extends Rejection


