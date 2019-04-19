package ru.tinkoff.tschema
package finagle

import Routed.implicits._
import cats.{Applicative, Functor}
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.applicative._
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.finagle.Rejection.missingParam
import ru.tinkoff.tschema.param.ParamSource.{All, Query}
import ru.tinkoff.tschema.param._
import ru.tinkoff.tschema.typeDSL.QueryParams
import shapeless._
import shapeless.labelled.{FieldType, field}

trait Serve[T, F[_], In, Out] {
  def process(in: In): F[Out]

  def as[U]: Serve[U, F, In, Out] = this.asInstanceOf[Serve[U, F, In, Out]]
}

object Serve extends ServeAuthInstances with ServeParamsInstances {
  import ru.tinkoff.tschema.typeDSL._
  type Filter[T, F[_], L]                 = Serve[T, F, L, L]
  type Add[T, F[_], In <: HList, name, V] = Serve[T, F, In, FieldType[name, V] :: In]

  def serveIgnore[T, F[_]: Applicative, In]: Filter[T, F, In]                          = in => in.pure[F]
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

  implicit def queryParamServe[F[_]: Routed, name: Name, x: Param.PQuery, In <: HList]
    : Add[QueryParam[name, x], F, In, name, x] =
    serveAdd[QueryParam[name, x], F, In, x, name](resolveParam[F, ParamSource.Query, name, x])

  implicit def queryFlagServe[F[_]: Routed, name <: Symbol: Name, x, In <: HList]: Add[QueryFlag[name], F, In, name, Boolean] =
    serveAdd[QueryFlag[name], F, In, Boolean, name](Routed.request[F].map(_.params.contains(Name[name].string)))

  implicit def captureServe[F[_]: Routed, name: Name, x: Param.PPath, In <: HList]: Add[Capture[name, x], F, In, name, x] =
    serveAdd[Capture[name, x], F, In, x, name](resolveParam[F, ParamSource.Path, name, x])

  implicit def headerServe[F[_]: Routed, name: Name, x: Param.PHeader, In <: HList]: Add[Header[name, x], F, In, name, x] =
    serveAdd[Header[name, x], F, In, x, name](resolveParam[F, ParamSource.Header, name, x])

  implicit def cookieServe[F[_]: Routed, name: Name, x: Param.PCookie, In <: HList]: Add[Cookie[name, x], F, In, name, x] =
    serveAdd[Cookie[name, x], F, In, x, name](resolveParam[F, ParamSource.Cookie, name, x])

  implicit def formFieldServe[F[_]: Routed, name <: Symbol: Witness.Aux, x: Param.PForm, In <: HList]
    : Add[FormField[name, x], F, In, name, x] =
    serveAdd[FormField[name, x], F, In, x, name](resolveParam[F, ParamSource.Form, name, x])

  implicit def bodyServe[F[_]: Functor, name <: Symbol: Witness.Aux, A, In <: HList](
      implicit A: ParseBody[F, A]): Add[ReqBody[name, A], F, In, name, A] =
    serveAdd[ReqBody[name, A], F, In, A, name](A.parse)

  implicit def prefix[F[_]: Routed, name: Name, In <: HList]: Filter[Prefix[name], F, In] =
    serveCheck(Routed.prefix(Name[name].string))

  implicit def serveKey[F[_]: Applicative, key, In]: Filter[Key[key], F, In] = serveIgnore
}

private[finagle] trait ServeParamsInstances { self: Serve.type =>

  implicit def queryParamsServe[F[_]: Routed, name: Name, x, In <: HList](implicit param: SingleParam[ParamSource.Query, x]) =
    serveAdd[QueryParams[name, x], F, In, List[x], name](extractQueryParams[F, name, x](allowEmpty = false))

  implicit def queryOptParamsServe[F[_]: Routed, name: Name, x, In <: HList](
      implicit param: SingleParam[ParamSource.Query, x]) =
    serveAdd[QueryParams[name, Option[x]], F, In, List[x], name](extractQueryParams[F, name, x](allowEmpty = true))

  private def extractQueryParams[F[_]: Routed, name: Name, x](allowEmpty: Boolean)(
      implicit param: SingleParam[ParamSource.Query, x]): F[List[x]] =
    Routed.request.flatMap { req =>
      val name = Name[name].string
      val dir  = ParamDirectives[Query]
      req.params.getAll(name) match {
        case it if it.nonEmpty || allowEmpty => it.toList.traverse(s => dir.provideOrReject(name, param.applyOpt(Some(s))))
        case _                               => Routed.reject(missingParam(name, Query))
      }
    }

}
