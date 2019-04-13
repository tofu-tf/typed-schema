package ru.tinkoff.tschema
package finagle

import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.instances.list._
import cats.{Applicative, Functor, Monad}
import com.twitter.finagle.http.Request
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.finagle.Rejection.{MalformedParam, MissingParam}
import ru.tinkoff.tschema.param.ParamSource.All
import ru.tinkoff.tschema.param._
import shapeless._
import shapeless.labelled.{FieldType, field}

trait Serve[T, F[_], In, Out] {
  def process(in: In): F[Out]

  def as[U]: Serve[U, F, In, Out] = this.asInstanceOf[Serve[U, F, In, Out]]
}

object Serve {
  type Filter[T, F[_], L]                 = Serve[T, F, L, L]
  type Add[T, F[_], In <: HList, name, V] = Serve[T, F, In, FieldType[name, V] :: In]

  def serveAdd[T, F[_]: Functor, In <: HList, A, key](fa: F[A]): Add[T, F, In, key, A] = in => fa.map(field[key](_) :: in)
  def serveCheck[T, F[_]: Functor, In <: HList](fa: F[Unit]): Filter[T, F, In]         = in => fa as in
  def serveCheckIn[T, F[_]: Functor, In <: HList](f: In => F[Unit]): Filter[T, F, In]  = in => f(in) as in
  def serveAddIn[T, F[_]: Functor, In <: HList, A, key](f: In => F[A]): Add[T, F, In, key, A] =
    in => f(in).map(field[key](_) :: in)

  protected def resolveParam[F[_]: Monad: Routed, S >: All <: ParamSource, name, A](
      implicit param: Param[S, A],
      w: Name[name],
      directives: ParamDirectives[S]
  ): F[A] =
    param match {
      case single: SingleParam[S, A] =>
        directives.getByName[F](w.string).flatMap(s => directives.provideOrReject[F, A](w.string, single.applyOpt(s)))
      case multi: MultiParam[S, A] =>
        multi.names
          .traverse(directives.getByName[F])
          .flatMap(ls => directives.provideOrReject[F, A](w.string, multi.applyOpt(ls)))
    }

}

trait ParamDirectives[S <: ParamSource] {
  def source: S
  def getByName[F[_]: Monad: Routed](name: String): F[Option[String]]

  def notFound(name: String): Rejection                 = MissingParam(name, source)
  def malformed(name: String, error: String): Rejection = MalformedParam(name, error, source)

  def singleRejection(name: String, error: SingleParamError): Rejection =
    error match {
      case MissingParamError    => notFound(name)
      case ParseParamError(err) => malformed(name, err)
    }

  def errorReject[F[_], A](name: String, error: ParamError)(implicit routed: Routed[F]): F[A] =
    error match {
      case single: SingleParamError => routed.reject(singleRejection(name, single))
      case MultiParamError(vals) =>
        routed.rejectMany(vals.map { case (field, err) => singleRejection(field, err) }.toSeq: _*)
    }

  def provideOrReject[F[_]: Applicative: Routed, A](name: String, result: Param.Result[A]): F[A] =
    result.fold(errorReject[F, A](name, _), _.pure[F])
}

abstract class ParamDirectivesSimple[S <: ParamSource](val source: S) extends ParamDirectives[S] {
  def getFromRequest(name: String)(req: Request): Option[String]

  def getByName[F[_]: Monad: Routed](name: String): F[Option[String]] =
    Routed.request[F].map(getFromRequest(name))
}

object ParamDirectives {
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
    def getByName[F[_]: Monad: Routed](name: String): F[Option[String]] = ???
    def source                                                          = ParamSource.Path
  }

  implicit val formDataParamDirectives: TC[Form] = new TCS[Form](Form) {
    def getFromRequest(name: String)(req: Request): Option[String] = ???
  }

  implicit val headerParamDirectives: TC[Header] = new TCS[Header](Header) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.headerMap.get(name)
  }

}
