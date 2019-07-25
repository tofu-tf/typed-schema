package ru.tinkoff.tschema
package finagle

import cats.arrow.Arrow
import cats.data.ContT
import cats.effect.Resource
import cats.free.Free
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.applicative._
import cats.{Applicative, Apply, Defer, FlatMap, Functor, Monad, StackSafeMonad}
import com.twitter.finagle.http
import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.finagle.Rejection.missingParam
import ru.tinkoff.tschema.param.ParamSource.{All, Query}
import ru.tinkoff.tschema.param._
import ru.tinkoff.tschema.typeDSL.{Delete, Get, Head, Options, Post, Put, QueryParams}
import ru.tinkoff.tschema.utils.cont
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.tools.nsc.ast.parser.Patch

trait Serve[T, F[_], In, Out] {
  def process(in: In, k: Out => F[Response]): F[Response]

  def as[U]: Serve[U, F, In, Out] = this.asInstanceOf[Serve[U, F, In, Out]]
}

object Serve
    extends ServeAuthInstances with ServeParamsInstances with ServeFunctions with ServeCatsInstance with ServeMethodInstances {
  import ru.tinkoff.tschema.typeDSL._
  type Filter[T, F[_], L]                 = Serve[T, F, L, L]
  type Add[T, F[_], In <: HList, name, V] = Serve[T, F, In, FieldType[name, V] :: In]

  implicit class ServeOps[T, F[_], In <: HList, Out](val serve: Serve[T, F, In, Out]) extends AnyVal {
    def add[key](implicit F: Functor[F]): Add[T, F, In, key, Out] = (in, f) => serve.process(in, a => f(field[key](a) :: in))
    def filter[key](implicit F: Functor[F]): Filter[T, F, In]     = (in, f) => serve.process(in, _ => f(in))

  }

  protected def resolveParam[F[_]: Routed: Monad, S >: All <: ParamSource, name, A](
      implicit param: Param[S, A],
      w: Name[name],
      directives: ParamDirectives[S],
  ): F[A] = param match {
    case single: SingleParam[S, A] =>
      directives
        .getByName[F, A](w.string, s => directives.provideOrReject[F, A](w.string, single.applyOpt(s)))
    case multi: MultiParam[S, A] =>
      cont.traverseCont[String, Option[CharSequence], A, F](multi.names)(directives.getByName)(
        ls => directives.provideOrReject(w.string, multi.applyOpt(ls))
      )

  }

  implicit def queryParamServe[F[_]: Routed: Monad, name: Name, x: Param.PQuery, In <: HList]
    : Add[QueryParam[name, x], F, In, name, x] =
    add[QueryParam[name, x], F, In, x, name](resolveParam[F, ParamSource.Query, name, x])

  implicit def queryFlagServe[F[_]: Routed: Monad, name <: Symbol: Name, x, In <: HList]
    : Add[QueryFlag[name], F, In, name, Boolean] =
    add[QueryFlag[name], F, In, Boolean, name](Routed.request[F].map(_.params.contains(Name[name].string)))

  implicit def captureServe[F[_]: Routed: Monad, name: Name, x: Param.PPath, In <: HList]
    : Add[Capture[name, x], F, In, name, x] =
    add[Capture[name, x], F, In, x, name](resolveParam[F, ParamSource.Path, name, x])

  implicit def headerServe[F[_]: Routed: Monad, name: Name, x: Param.PHeader, In <: HList]
    : Add[Header[name, x], F, In, name, x] =
    add[Header[name, x], F, In, x, name](resolveParam[F, ParamSource.Header, name, x])

  implicit def cookieServe[F[_]: Routed: Monad, name: Name, x: Param.PCookie, In <: HList]
    : Add[Cookie[name, x], F, In, name, x] =
    add[Cookie[name, x], F, In, x, name](resolveParam[F, ParamSource.Cookie, name, x])

  implicit def formFieldServe[F[_]: Routed: Monad, name <: Symbol: Witness.Aux, x: Param.PForm, In <: HList]
    : Add[FormField[name, x], F, In, name, x] =
    add[FormField[name, x], F, In, x, name](resolveParam[F, ParamSource.Form, name, x])

  implicit def bodyServe[F[_]: FlatMap, name <: Symbol: Witness.Aux, A, In <: HList](
      implicit A: ParseBody[F, A]): Add[ReqBody[name, A], F, In, name, A] =
    add[ReqBody[name, A], F, In, A, name](A.parse())

  implicit def prefix[F[_]: Routed: Monad, name: Name, In <: HList]: Filter[Prefix[name], F, In] =
    checkCont(Routed.checkPrefix(Name[name].string, _))

  implicit def serveKey[F[_], key, In]: Filter[Key[key], F, In]     = ignore
  implicit def serveGroup[F[_], key, In]: Filter[Group[key], F, In] = ignore
  implicit def serveMeta[F[_], U <: Meta, In]: Filter[U, F, In]     = ignore

  implicit def asServe[x, name, F[_], In <: HList, Head, old, Rest <: HList](
      implicit serveInner: Serve[x, F, In, FieldType[old, Head] :: Rest])
    : Serve[As[x, name], F, In, FieldType[name, Head] :: Rest] =
    serveInner.asInstanceOf[Serve[As[x, name], F, In, FieldType[name, Head] :: Rest]]
}

private[finagle] trait ServeParamsInstances { self: Serve.type =>

  implicit def queryParamsServe[F[_]: Routed: Monad, name: Name, x, In <: HList](
      implicit param: SingleParam[ParamSource.Query, x]) =
    add[QueryParams[name, x], F, In, List[x], name](extractQueryParams[F, name, x](allowEmpty = false))

  implicit def queryOptParamsServe[F[_]: Routed: Monad, name: Name, x, In <: HList](
      implicit param: SingleParam[ParamSource.Query, x]) =
    add[QueryParams[name, Option[x]], F, In, List[x], name](extractQueryParams[F, name, x](allowEmpty = true))

  private def extractQueryParams[F[_]: Routed: Monad, name: Name, x](allowEmpty: Boolean)(
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

  def provide[T, F[_]: FlatMap, In, A](fa: => F[A]): Serve[T, F, In, A]      = (_, f) => fa.flatMap(a => f(a))
  def provideIn[T, F[_]: FlatMap, In, A](fa: In => F[A]): Serve[T, F, In, A] = (in, f) => fa(in).flatMap(f)

  def check[T, F[_]: Apply, In, A](fu: F[A]): Filter[T, F, In]                        = (in, f) => fu *> f(in)
  def checkCont[T, F[_]: Apply, In](fr: F[Response] => F[Response]): Filter[T, F, In] = (in, f) => fr(f(in))
  def checkIn[T, F[_]: Apply, In](f: In => F[Unit]): Filter[T, F, In]                 = (in, g) => f(in) *> g(in)

  def add[T, F[_]: FlatMap, In <: HList, A, key](fa: F[A]): Add[T, F, In, key, A]        = provide[T, F, In, A](fa).add[key]
  def addIn[T, F[_]: FlatMap, In <: HList, A, key](f: In => F[A]): Add[T, F, In, key, A] = provideIn[T, F, In, A](f).add[key]

  def resource[T, F[_]: BracketThrow, In, A, key](r: Resource[F, A]): Serve[T, F, In, A]         = (_, f) => r.use(f)
  def resourceIn[T, F[_]: BracketThrow, In, A, key](r: In => Resource[F, A]): Serve[T, F, In, A] = r(_).use(_)

  def respond[T, F[_], In, Out](r: F[Response]): Serve[T, F, In, Out]         = (_, _) => r
  def respondIn[T, F[_], In, Out](r: In => F[Response]): Serve[T, F, In, Out] = (in, _) => r(in)
}

private[finagle] trait ServeCatsInstance {
  implicit def serveMonad[T, F[_], In](implicit FD: Defer[F]): Monad[Serve[T, F, In, *]] =
    new StackSafeMonad[Serve[T, F, In, *]] {
      def flatMap[A, B](fa: Serve[T, F, In, A])(f: A => Serve[T, F, In, B]): Serve[T, F, In, B] =
        (in, k) => FD.defer(fa.process(in, a => f(a).process(in, k)))
      def pure[A](x: A): Serve[T, F, In, A] = (_, k) => k(x)
    }

  implicit def serveArrow[T, F[_], In](implicit FD: Defer[F]): Arrow[Serve[T, F, *, *]] =
    new Arrow[Serve[T, F, *, *]] {
      def lift[A, B](f: A => B): Serve[T, F, A, B] = (a, kb) => FD.defer(kb(f(a)))
      def compose[A, B, C](f: Serve[T, F, B, C], g: Serve[T, F, A, B]): Serve[T, F, A, C] =
        (a, kc) => FD.defer(g.process(a, b => f.process(b, kc)))
      def first[A, B, C](fa: Serve[T, F, A, B]): Serve[T, F, (A, C), (B, C)] =
        (ac, kbc) => FD.defer(fa.process(ac._1, b => kbc((b, ac._2))))
    }
}
trait ServeMethodInstances { self: Serve.type =>
  private[this] def checkMethod[T, F[_]: Routed: Monad, In](method: http.Method): Filter[T, F, In] =
    check(Routed.request.flatMap(r => Routed.reject(Rejection.wrongMethod(r.method.name)).whenA(r.method != method)))

  implicit def serveMethodGet[F[_]: Routed: Monad, In]: Filter[Get, F, In]         = checkMethod(http.Method.Get)
  implicit def serveMethodPost[F[_]: Routed: Monad, In]: Filter[Post, F, In]       = checkMethod(http.Method.Post)
  implicit def serveMethodPut[F[_]: Routed: Monad, In]: Filter[Put, F, In]         = checkMethod(http.Method.Put)
  implicit def serveMethodDelete[F[_]: Routed: Monad, In]: Filter[Delete, F, In]   = checkMethod(http.Method.Delete)
  implicit def serveMethodHead[F[_]: Routed: Monad, In]: Filter[Head, F, In]       = checkMethod(http.Method.Head)
  implicit def serveMethodOptions[F[_]: Routed: Monad, In]: Filter[Options, F, In] = checkMethod(http.Method.Options)
  implicit def serveMethodPatch[F[_]: Routed: Monad, In]: Filter[Patch, F, In]     = checkMethod(http.Method.Patch)
}
