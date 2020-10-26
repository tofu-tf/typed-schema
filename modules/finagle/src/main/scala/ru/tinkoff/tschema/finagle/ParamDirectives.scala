package ru.tinkoff.tschema
package finagle

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.http.exp.Multipart
import ru.tinkoff.tschema.param._
import shapeless.{Witness => W, _}
import shapeless.labelled.{FieldType, field}

import ParamDirectives.multipartKey

trait ParamDirectives[S <: ParamSource] {
  def source: S
  def getByName[F[_]: Routed: Monad, A](name: String, fa: Option[CharSequence] => F[A]): F[A]

  def notFound(name: String): Rejection                 = Rejection.missingParam(name, source)
  def malformed(name: String, error: String): Rejection = Rejection.malformedParam(name, error, source)

  def singleRejection(name: String, error: SingleParamError): Rejection =
    error match {
      case MissingParamError    => notFound(name)
      case ParseParamError(err) => malformed(name, err)
    }

  def errorReject[F[_], A](name: String, error: ParamError)(implicit routed: Routed[F]): F[A] =
    error match {
      case single: SingleParamError => routed.reject(singleRejection(name, single))
      case MultiParamError(vals)    =>
        Routed.rejectMany(vals.map { case (field, err) => singleRejection(field, err) }.toSeq: _*)
    }

  def direct[F[_]: Routed: Monad, A, name, In <: HList](
      name: String,
      result: Param.Result[A],
      in: In,
      k: (FieldType[name, A] :: In) => F[Response]
  ): F[Response] =
    result.fold(errorReject[F, Response](name, _), a => k(field[name](a) :: in))

  def direct[F[_]: Routed: Monad, A, name, In <: HList](
      name: String,
      result: Param.Result[A],
      multipart: Multipart,
      in: In,
      k: (FieldType[multipartKey, Multipart] :: FieldType[name, A] :: In) => F[Response]
  ): F[Response] =
    result.fold(errorReject[F, Response](name, _), a => k(field[multipartKey](multipart) ::field[name](a) :: in))

  def provideOrReject[F[_]: Routed: Monad, A](name: String, result: Param.Result[A]): F[A] =
    result.fold(errorReject[F, A](name, _), _.pure[F])
}

abstract class ParamDirectivesSimple[S <: ParamSource](val source: S) extends ParamDirectives[S] {
  def getFromRequest(name: String)(req: Request): Option[CharSequence]

  def getByName[F[_]: Routed: Monad, A](name: String, f: Option[CharSequence] => F[A]): F[A] =
    Routed.request.flatMap(req => f(getFromRequest(name)(req)))
}

object ParamDirectives {
  def apply[S <: ParamSource](implicit dir: ParamDirectives[S]): ParamDirectives[S] = dir

  type multipartKey = W.`"multipart"`.T
  type TC[A <: ParamSource]  = ParamDirectives[A]
  type TCS[A <: ParamSource] = ParamDirectivesSimple[A]
  import ParamSource._

  implicit val queryParamDirective: TC[Query] = new TCS[Query](Query) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.params.get(name)
  }

  implicit val cookieParamDirectives: TC[Cookie] = new TCS[Cookie](Cookie) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.cookies.get(name).map(_.value)
  }

  implicit val pathParamDirectives: TC[ParamSource.Path] = new TC[ParamSource.Path] {
    def getByName[F[_]: Routed: Monad, A](name: String, fa: Option[CharSequence] => F[A]): F[A] = Routed.segment(fa)
    def source                                                                                  = ParamSource.Path
  }

  implicit val formDataParamDirectives: TC[Form] = new TCS[Form](Form) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.params.get(name)
  }

  implicit def multipartFieldParamDirectives(
    implicit multipart: Multipart
  ): TC[MultipartField] = new TC[MultipartField] {
    val source: MultipartField = MultipartField
    def getByName[F[_]: Routed: Monad, A](name: String, fa: Option[CharSequence] => F[A]): F[A] =
      fa(multipart.attributes.get(name).flatMap(_.headOption))
  }

  implicit val headerParamDirectives: TC[Header] = new TCS[Header](Header) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.headerMap.get(name)
  }

}
