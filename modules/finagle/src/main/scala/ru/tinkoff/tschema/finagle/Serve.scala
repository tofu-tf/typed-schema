package ru.tinkoff.tschema
package finagle

import Routed.implicits._
import cats.effect.{Bracket, Resource}
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Apply, FlatMap, Functor, Monad}
import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.finagle.Rejection.missingParam
import ru.tinkoff.tschema.finagle.util.cont.traverseCont
import ru.tinkoff.tschema.param.ParamSource.{All, Query}
import ru.tinkoff.tschema.param._
import ru.tinkoff.tschema.typeDSL.QueryParams
import shapeless._
import shapeless.labelled.{FieldType, field}

trait Serve[T, F[_], In, Out] {
  def process(in: In, k: Out => F[Response]): F[Response]

  def as[U]: Serve[U, F, In, Out] = this.asInstanceOf[Serve[U, F, In, Out]]
}

object Serve extends ServeAuthInstances with ServeParamsInstances with ServeFunctions with ServeMonadInstance {
  import ru.tinkoff.tschema.typeDSL._
  type Filter[T, F[_], L]                 = Serve[T, F, L, L]
  type Add[T, F[_], In <: HList, name, V] = Serve[T, F, In, FieldType[name, V] :: In]

  implicit class ServeOps[T, F[_], In <: HList, Out](val serve: Serve[T, F, In, Out]) extends AnyVal {
    def add[key](implicit F: Functor[F]): Add[T, F, In, key, Out] = (in, f) => serve.process(in, a => f(field[key](a) :: in))
    def filter[key](implicit F: Functor[F]): Filter[T, F, In]     = (in, f) => serve.process(in, _ => f(in))

  }

  protected def resolveParam[F[_], S >: All <: ParamSource, name, A](
      implicit param: Param[S, A],
      w: Name[name],
      directives: ParamDirectives[S],
      routed: Routed[F]
  ): F[A] = param match {
    case single: SingleParam[S, A] =>
      directives.getByName[F, A](w.string, s => directives.provideOrReject[F, A](w.string, single.applyOpt(s)))
    case multi: MultiParam[S, A] =>
      traverseCont[F, A, String, Option[CharSequence]](multi.names)((name, k) => directives.getByName(name, k))(
        ls => directives.provideOrReject(w.string, multi.applyOpt(ls))
      )
  }

  implicit def queryParamServe[F[_]: Routed, name: Name, x: Param.PQuery, In <: HList]
    : Add[QueryParam[name, x], F, In, name, x] =
    add[QueryParam[name, x], F, In, x, name](resolveParam[F, ParamSource.Query, name, x])

  implicit def queryFlagServe[F[_]: Routed, name <: Symbol: Name, x, In <: HList]: Add[QueryFlag[name], F, In, name, Boolean] =
    add[QueryFlag[name], F, In, Boolean, name](Routed.request[F].map(_.params.contains(Name[name].string)))

  implicit def captureServe[F[_]: Routed, name: Name, x: Param.PPath, In <: HList]: Add[Capture[name, x], F, In, name, x] =
    add[Capture[name, x], F, In, x, name](resolveParam[F, ParamSource.Path, name, x])

  implicit def headerServe[F[_]: Routed, name: Name, x: Param.PHeader, In <: HList]: Add[Header[name, x], F, In, name, x] =
    add[Header[name, x], F, In, x, name](resolveParam[F, ParamSource.Header, name, x])

  implicit def cookieServe[F[_]: Routed, name: Name, x: Param.PCookie, In <: HList]: Add[Cookie[name, x], F, In, name, x] =
    add[Cookie[name, x], F, In, x, name](resolveParam[F, ParamSource.Cookie, name, x])

  implicit def formFieldServe[F[_]: Routed, name <: Symbol: Witness.Aux, x: Param.PForm, In <: HList]
    : Add[FormField[name, x], F, In, name, x] =
    add[FormField[name, x], F, In, x, name](resolveParam[F, ParamSource.Form, name, x])

  implicit def bodyServe[F[_]: FlatMap, name <: Symbol: Witness.Aux, A, In <: HList](
      implicit A: ParseBody[F, A]): Add[ReqBody[name, A], F, In, name, A] =
    add[ReqBody[name, A], F, In, A, name](A.parse)

  implicit def prefix[F[_]: Routed, name: Name, In <: HList]: Filter[Prefix[name], F, In] =
    checkCont(Routed.prefix(Name[name].string, _))

  implicit def serveKey[F[_]: Applicative, key, In]: Filter[Key[key], F, In] = ignore
}

private[finagle] trait ServeParamsInstances { self: Serve.type =>

  implicit def queryParamsServe[F[_]: Routed, name: Name, x, In <: HList](implicit param: SingleParam[ParamSource.Query, x]) =
    add[QueryParams[name, x], F, In, List[x], name](extractQueryParams[F, name, x](allowEmpty = false))

  implicit def queryOptParamsServe[F[_]: Routed, name: Name, x, In <: HList](
      implicit param: SingleParam[ParamSource.Query, x]) =
    add[QueryParams[name, Option[x]], F, In, List[x], name](extractQueryParams[F, name, x](allowEmpty = true))

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

private[finagle] trait ServeFunctions { self: Serve.type =>

  def ignore[T, F[_], In]: Filter[T, F, In]                         = (in, f) => f(in)
  def pure[T, F[_]: FlatMap, In, A](a: A): Serve[T, F, In, A]       = (_, f) => f(a)
  def read[T, F[_]: FlatMap, In, A](a: In => A): Serve[T, F, In, A] = (in, f) => f(a(in))

  def provide[T, F[_]: FlatMap, In, A](fa: F[A]): Serve[T, F, In, A]         = (_, f) => fa.flatMap(f)
  def provideIn[T, F[_]: FlatMap, In, A](fa: In => F[A]): Serve[T, F, In, A] = (in, f) => fa(in).flatMap(f)

  def check[T, F[_]: Apply, In <: HList, A](fu: F[A]): Filter[T, F, In]                        = (in, f) => fu *> f(in)
  def checkCont[T, F[_]: Apply, In <: HList](fr: F[Response] => F[Response]): Filter[T, F, In] = (in, f) => fr(f(in))
  def checkIn[T, F[_]: Apply, In <: HList](f: In => F[Unit]): Filter[T, F, In]                 = (in, g) => f(in) *> g(in)

  def add[T, F[_]: FlatMap, In <: HList, A, key](fa: F[A]): Add[T, F, In, key, A]        = provide[T, F, In, A](fa).add[key]
  def addIn[T, F[_]: FlatMap, In <: HList, A, key](f: In => F[A]): Add[T, F, In, key, A] = provideIn[T, F, In, A](f).add[key]

  def resource[T, F[_]: BracketThrow, In <: HList, A, key](r: Resource[F, A]): Serve[T, F, In, A]         = (_, f) => r.use(f)
  def resourceIn[T, F[_]: BracketThrow, In <: HList, A, key](r: In => Resource[F, A]): Serve[T, F, In, A] = r(_).use(_)

  def respond[T, F[_], In, Out](r: F[Response]): Serve[T, F, In, Out]         = (_, _) => r
  def respondIn[T, F[_], In, Out](r: In => F[Response]): Serve[T, F, In, Out] = (in, _) => r(in)
}

private[finagle] trait ServeMonadInstance {
  implicit def serveMonad[T, F[_]: Monad, In]: Monad[Serve[T, F[_], In, ?]] = new Monad[Serve[T, F[_], In, ?]] {
    def flatMap[A, B](fa: Serve[T, F[_], In, A])(f: A => Serve[T, F[_], In, B]): Serve[T, F[_], In, B] = ???
    def tailRecM[A, B](a: A)(f: A => Serve[T, F[_], In, Either[A, B]]): Serve[T, F[_], In, B]          = ???
    def pure[A](x: A): Serve[T, F[_], In, A]                                                           = Serve.provide()
  }
}
