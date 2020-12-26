package ru.tinkoff.tschema.http4s

import cats.data.OptionT
import cats.data.OptionT.none
import cats.effect.Sync
import cats.syntax.traverse._
import cats.{ Applicative, Defer }
import org.http4s.dsl.request._
import org.http4s.{ EntityDecoder, Method }
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.http4s.Routing.Routes
import ru.tinkoff.tschema.param.ParamSource.{ All, Query }
import ru.tinkoff.tschema.param.{ MultiParam, Param, ParamSource, SingleParam }
import ru.tinkoff.tschema.typeDSL._
import shapeless._
import shapeless.labelled.{ FieldType, field }
import tofu.MonadThrow

import scala.collection.immutable

trait Serve[T, F[_], In, Out] {
  def process(k: Out => Routes[F], in: In): Routes[F]

  def as[U]: Serve[U, F, In, Out] = this.asInstanceOf[Serve[U, F, In, Out]]
}

object Serve extends ServeInstances with ServeMethodInstances

trait ServeInstances {

  type Filter[T, F[_], L]                 = Serve[T, F, L, L]
  type Add[T, F[_], In <: HList, name, V] = Serve[T, F, In, FieldType[name, V] :: In]
  type Push[T, F[_], In <: HList, value]  = Serve[T, F, In, value :: In]

  implicit def allQueryServe[F[_] : Defer : Applicative, name, In <: HList]
    : Add[AllQuery[name], F, In, name, Map[String, String]] =
    (k, in) => Routing[F] { req => k(field[name](req.queryParams) :: in)(req) }

  implicit def captureServe[F[_] : Sync, name: Name, p, x: Param.PPath, In <: HList]
    : Add[CaptureAs[name, p, x], F, In, p, x] =
    resolveParam[F, ParamSource.Path, name, p, x, CaptureAs[name, p, x], In]

  implicit def queryParamServe[F[_] : Sync, name: Name, p, x: Param.PQuery, In <: HList]
    : Add[QueryParamAs[name, p, x], F, In, p, x] =
    resolveParam[F, ParamSource.Query, name, p, x, QueryParamAs[name, p, x], In]

  implicit def queryParamsServe[F[_] : Sync, name: Name, p, x, In <: HList](
    implicit param: SingleParam[ParamSource.Query, x]
  ): Add[QueryParamsAs[name, p, x], F, In, name, List[x]] =
    extractQueryParams[QueryParamsAs[name, p, x], F, In, name, x](allowEmpty = false)

  implicit def queryOptParamsServe[F[_] : Sync, name: Name, p, x, In <: HList](
    implicit param: SingleParam[ParamSource.Query, x]
  ): Add[QueryParamsAs[name, p, Option[x]], F, In, name, List[x]] =
    extractQueryParams[QueryParamsAs[name, p, Option[x]], F, In, name, x](allowEmpty = true)

  implicit def queryFlagServe[F[_] : Defer : Applicative, name: Name, p, In <: HList]
    : Add[QueryFlagAs[name, p], F, In, p, Boolean] =
    (k, in) => Routing[F] { req => k(field[p](req.queryParams.contains(Name[name].string)) :: in)(req) }

  implicit def reqBodyServe[F[_] : Defer : MonadThrow, name: Name, p, x : EntityDecoder[F, *], In <: HList]
    : Add[ReqBodyAs[name, p, x], F, In, p, x] =
    (k, in) => Routing[F] { req =>
      for {
        body <- OptionT.liftF(req.body)
        a    <- k(field[p](body) :: in)(req)
      } yield a
    }

  implicit def headerServe[F[_] : Sync, name: Name, p, x: Param.PHeader, In <: HList]
    : Add[HeaderAs[name, p, x], F, In, p, x] =
    resolveParam[F, ParamSource.Header, name, p, x, HeaderAs[name, p, x], In]

  implicit def formFieldServe[F[_] : Sync, name: Name, p, x: Param.PForm, In <: HList]
    : Add[FormFieldAs[name, p, x], F, In, p, x] =
    resolveParam[F, ParamSource.Form, name, p, x, FormFieldAs[name, p, x], In]

  implicit def cookieServe[F[_] : Sync, name: Name, p, x: Param.PCookie, In <: HList]
    : Add[CookieAs[name, p, x], F, In, p, x] =
    resolveParam[F, ParamSource.Cookie, name, p, x, CookieAs[name, p, x], In]

  implicit def prefix[F[_] : Defer : Applicative, name: Name, In <: HList]: Filter[Prefix[name], F, In] =
    (k, in) => Routing[F] { _.matchPrefix(Name[name].string).map(k(in)(_)).getOrElse(none) }

  implicit def serveKey[F[_], key, In <: HList]: Push[Key[key], F, In, Key[key]] = (k, in) => k(Key.of[key] :: in)
  implicit def serveGroup[F[_], key, In <: HList]: Push[Group[key], F, In, Group[key]] = (k, in) => k(Group.of[key] :: in)

  implicit def serveMeta[F[_], U <: Meta, In]: Filter[U, F, In] = ignore

  private def ignore[T, F[_], In]: Filter[T, F, In] = _(_)

  private def resolveParam[F[_] : Sync, S >: All <: ParamSource, name, p, A, D, In <: HList](implicit
    param: Param[S, A],
    w: Name[name],
    directives: ParamDirectives[S]
  ): Add[D, F, In, p, A] =
    param match {
      case s: SingleParam[S, A] =>
        (k, in) => directives.getByName[F](w.string, v => directives.direct(s.applyOpt(v), in, k))
      case m: MultiParam[S, A]   =>
        (k, in) => traverseCont[F](m.names)(directives.getByName)(lv => directives.direct(m.applyOpt(lv), in, k))
    }

  private def extractQueryParams[T, F[_] : Sync, In <: HList, name: Name, x](allowEmpty: Boolean)(
    implicit param: SingleParam[ParamSource.Query, x]
  ): Add[T, F, In, name, List[x]] = (k, in) => {
    val name = Name[name].string
    val dir  = ParamDirectives[Query]
    dir.getAllByName(name, {
      case list if list.nonEmpty || allowEmpty =>
        dir.direct(list.map(v => param.applyOpt(Some(v))).sequence, in, k)
      case _ => Routing.empty
    })
  }

  private def traverseCont[F[_] : Defer : Applicative](
    items: List[String]
  )(cont: (String, Option[String] => Routes[F]) => Routes[F])(k: List[Option[String]] => Routes[F]): Routes[F] = {
    def go(as: List[String], agg: List[Option[String]]): Routes[F] =
      as match {
        case immutable.::(a, rest) => Routing[F] { req => cont(a, b => go(rest, b :: agg))(req) }
        case Nil => k(agg.reverse)
      }
    go(items, Nil)
  }
}

trait ServeMethodInstances               { self: Serve.type =>
  implicit def serveMethod[method, F[_] : Defer : Applicative, In](
    implicit check: MethodCheck[method]
  ): Filter[method, F, In] = (k, in) =>
    Routing[F] { routed =>
      val METHOD: Method = check.method
      routed.request match {
        case METHOD -> _ => k(in)(routed)
        case _ => none
      }
    }

  private[this] final case class MethodCheck[T](method: Method)
  private[this] object MethodCheck {
    implicit val checkGet: MethodCheck[Get]         = MethodCheck(Method.GET)
    implicit val checkPost: MethodCheck[Post]       = MethodCheck(Method.POST)
    implicit val checkDelete: MethodCheck[Delete]   = MethodCheck(Method.DELETE)
    implicit val checkPut: MethodCheck[Put]         = MethodCheck(Method.PUT)
    implicit val checkOptions: MethodCheck[Options] = MethodCheck(Method.OPTIONS)
    implicit val checkHead: MethodCheck[Head]       = MethodCheck(Method.HEAD)
    implicit val checkPatch: MethodCheck[Patch]     = MethodCheck(Method.PATCH)
  }
}
