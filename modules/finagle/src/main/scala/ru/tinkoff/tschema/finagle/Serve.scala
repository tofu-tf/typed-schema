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
import Routed.implicits._

trait Serve[T, F[_], In, Out] {
  def process(in: In): F[Out]

  def as[U]: Serve[U, F, In, Out] = this.asInstanceOf[Serve[U, F, In, Out]]
}

object Serve {
  import ru.tinkoff.tschema.typeDSL._
  type Filter[T, F[_], L]                 = Serve[T, F, L, L]
  type Add[T, F[_], In <: HList, name, V] = Serve[T, F, In, FieldType[name, V] :: In]

  def serveAdd[T, F[_]: Functor, In <: HList, A, key](fa: F[A]): Add[T, F, In, key, A] = in => fa.map(field[key](_) :: in)
  def serveCheck[T, F[_]: Functor, In <: HList](fa: F[Unit]): Filter[T, F, In]         = in => fa as in
  def serveCheckIn[T, F[_]: Functor, In <: HList](f: In => F[Unit]): Filter[T, F, In]  = in => f(in) as in
  def serveAddIn[T, F[_]: Functor, In <: HList, A, key](f: In => F[A]): Add[T, F, In, key, A] =
    in => f(in).map(field[key](_) :: in)

  protected def resolveParam[F[_], S >: All <: ParamSource, name, A](
      implicit param: Param[S, A],
      w: Name[name],
      directives: ParamDirectives[S],
      routed: Routed[F]
  ): F[A] = param match {
    case single: SingleParam[S, A] =>
      directives.getByName[F](w.string).flatMap(s => directives.provideOrReject[F, A](w.string, single.applyOpt(s)))
    case multi: MultiParam[S, A] =>
      multi.names
        .traverse(directives.getByName[F])
        .flatMap(ls => directives.provideOrReject[F, A](w.string, multi.applyOpt(ls)))
  }

  implicit def queryParamServe[F[_]: Routed, name: Name, x: Param.PQuery, In <: HList] =
    serveAdd[QueryParam[name, x], F, In, x, name](resolveParam[F, ParamSource.Query, name, x])

  implicit def captureServe[F[_]: Routed, name: Name, x: Param.PPath, In <: HList] =
    serveAdd[Capture[name, x], F, In, x, name](resolveParam[F, ParamSource.Path, name, x])

  implicit def headerServe[F[_]: Routed, name: Name, x: Param.PHeader, In <: HList] =
    serveAdd[Header[name, x], F, In, x, name](resolveParam[F, ParamSource.Header, name, x])

  implicit def cookieServe[F[_]: Routed, name: Name, x: Param.PCookie, In <: HList] =
    serveAdd[Cookie[name, x], F, In, x, name](resolveParam[F, ParamSource.Cookie, name, x])

  implicit def formFieldServe[F[_]: Routed, name <: Symbol: Witness.Aux, x: Param.PForm, In <: HList] =
    serveAdd[FormField[name, x], F, In, x, name](resolveParam[F, ParamSource.Form, name, x])

  implicit def bodyServe[F[_]: Functor, name <: Symbol: Witness.Aux, A, In <: HList](implicit A: ParseBody[F, A]) =
    serveAdd[ReqBody[name, A], F, In, A, name](A.parse)

  implicit def prefix[F[_]: Routed, name: Name]: F[Unit] = Routed.prefix(Name[name].string)
}

trait ParamDirectives[S <: ParamSource] {
  def source: S
  def getByName[F[_]: Routed](name: String): F[Option[String]]

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

  def provideOrReject[F[_]: Routed, A](name: String, result: Param.Result[A]): F[A] =
    result.fold(errorReject[F, A](name, _), _.pure[F])
}

abstract class ParamDirectivesSimple[S <: ParamSource](val source: S) extends ParamDirectives[S] {
  def getFromRequest(name: String)(req: Request): Option[String]

  def getByName[F[_]](name: String)(implicit F: Routed[F]): F[Option[String]] = {
    F.FMonad.map(Routed.request[F])(getFromRequest(name))
  }
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
    def getByName[F[_]: Routed](name: String): F[Option[String]] = Routed.segment.map(_.map(_.toString))
    def source                                                   = ParamSource.Path
  }

  implicit val formDataParamDirectives: TC[Form] = new TCS[Form](Form) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.params.get(name)
  }

  implicit val headerParamDirectives: TC[Header] = new TCS[Header](Header) {
    def getFromRequest(name: String)(req: Request): Option[String] = req.headerMap.get(name)
  }

}
