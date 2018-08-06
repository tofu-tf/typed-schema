package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.model.{HttpMethod, HttpMethods}
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import cats.data.OptionT
import ru.tinkoff.tschema._
import ru.tinkoff.tschema.typeDSL._
import shapeless._
import shapeless.labelled.{FieldType, field}
import catsInstances._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.option._
import ru.tinkoff.tschema.akkaHttp.auth.{BasicAuthenticator, BearerAuthenticator}
import ru.tinkoff.tschema.common.Name
import shapeless.ops.hlist.Selector

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}

@implicitNotFound("could not serve ${T} using input ${In} ")
trait Serve[T, In <: HList] {
  type Out <: HList
  def directive(in: In): Directive1[Out]

  def as[Q]: Serve.Aux[Q, In, Out] = asInstanceOf[Serve.Aux[Q, In, Out]]
}

object Serve extends ServeInstances

private[akkaHttp] trait ServeTypes {
  type Aux[T, In <: HList, O <: HList] = Serve[T, In] { type Out = O }
  class key[name] protected[akkaHttp] ()
}

private[akkaHttp] trait ServeFunctions extends ServeTypes {
  protected def tryParse[T, F[x] <: FromParam[x], name <: Symbol](value: String)(implicit parse: F[T],
                                                                                 w: Witness.Aux[name]): Directive1[T] =
    Directive[Tuple1[T]](f =>
      parse(value) match {
        case Right(result) => f(Tuple1(result))
        case Left(err)     => reject(ParamFormatRejection(w.value.name, err))
    })

  protected def tryParseOpt[T, F[x] <: FromParam[x], name <: Symbol](
      value: String)(implicit parse: F[T], w: Witness.Aux[name]): Directive1[Option[T]] =
    Directive[Tuple1[Option[T]]](f =>
      parse(value) match {
        case Right(result) => f(Tuple1(Some(result)))
        case Left(err)     => f(Tuple1(None))
    })

  protected def name[name <: Symbol](implicit w: Witness.Aux[name]): String = w.value.name

  def serveAdd[T, In <: HList, A, key](dir: Directive1[A]): Aux[T, In, FieldType[key, A] :: In] = new Serve[T, In] {
    type Out = FieldType[key, A] :: In
    def directive(in: In): Directive1[FieldType[key, A] :: In] = dir.map(field[key](_) :: in)
  }

  def serveCheck[T, In <: HList](dir: Directive0): Aux[T, In, In] = new Serve[T, In] {
    type Out = In
    def directive(in: In): Directive1[In] = dir.tmap(_ => in)
  }

  def serveMap[T, In <: HList, nameA, nameB, A, B](f: A => B)(
      implicit select: Selector[In, FieldType[nameA, A]]): Aux[T, In, FieldType[nameB, B] :: In] = new Serve[T, In] {
    type Out = FieldType[nameB, B] :: In
    def directive(in: In): Directive1[Out] = provide(field[nameB](f(select(in))) :: in)
  }

  def serveMap2[T, In <: HList, nameA, nameB, nameC, A, B, C](f: (A, B) => C)(
      implicit selectA: Selector[In, FieldType[nameA, A]],
      selectB: Selector[In, FieldType[nameB, B]]): Aux[T, In, FieldType[nameC, C] :: In] = new Serve[T, In] {
    type Out = FieldType[nameC, C] :: In
    def directive(in: In): Directive1[Out] = provide(field[nameC](f(selectA(in), selectB(in))) :: in)
  }

  def serveFMap[T, In <: HList, nameA, nameB, A, B](f: A => Future[B])(
      implicit select: Selector[In, FieldType[nameA, A]],
      ec: ExecutionContext): Aux[T, In, FieldType[nameB, B] :: In] = new Serve[T, In] {
    type Out = FieldType[nameB, B] :: In
    def directive(in: In): Directive1[Out] = Directive { handle => ctx =>
      for {
        b   <- f(select(in): A)
        out = field[nameB](b) :: in
        res <- handle(Tuple1(out))(ctx)
      } yield res
    }
  }

  def serveFilter[T, In <: HList, name, A](f: A => Option[Rejection])(
      implicit select: Selector[In, FieldType[name, A]]): Aux[T, In, In] = new Serve[T, In] {
    type Out = In
    def directive(in: In): Directive1[In] = Directive { handle =>
      f(select(in)) match {
        case Some(rej) => reject(rej)
        case None      => handle(Tuple1(in))
      }
    }
  }
}

private[akkaHttp] trait ServeInstances extends ServeFunctions with ServeInstances1 { self: Serve.type =>
  implicit def prefixServe[pref, In <: HList](implicit n: Name[pref]) = serveCheck[Prefix[pref], In](pathPrefix(n.string))

  implicit def methodServe[method, In <: HList](implicit check: MethodCheck[method]) =
    serveCheck[method, In](method(check.method))

  implicit def queryParamServe[name <: Symbol: Witness.Aux, x: FromQueryParam, In <: HList] =
    serveAdd[QueryParam[name, x], In, x, name](
      parameter(name[name]).flatMap(param => tryParse[x, FromQueryParam, name](param))
    )

  implicit def queryOptParamsServe[name <: Symbol: Witness.Aux, x: FromQueryParam, In <: HList] =
    serveAdd[QueryParams[name, Option[x]], In, List[x], name] {
      parameterMultiMap.flatMap(
        _.getOrElse(name[name], Nil).traverse[Directive1, x](value => tryParse[x, FromQueryParam, name](value))
      )
    }

  implicit def queryOptParamServe[name <: Symbol: Witness.Aux, x: FromQueryParam, In <: HList] =
    serveAdd[QueryParam[name, Option[x]], In, Option[x], name](
      OptionT[Directive1, String](parameter(name[name].?)).flatMap { param =>
        OptionT(tryParseOpt[x, FromQueryParam, name](param))
      }.value
    )

  implicit def queryFlagServe[name <: Symbol: Witness.Aux, x, In <: HList] = serveAdd[QueryFlag[name], In, Boolean, name](
    parameterMap.map(_.contains(name[name]))
  )

  implicit def captureServe[name: Witness.Aux, x, In <: HList](implicit fromPathParam: FromPathParam[x]) =
    serveAdd[Capture[name, x], In, x, name] {
      pathPrefix(fromPathParam.matcher)
    }

  implicit def reqBodyServe[name: Witness.Aux, x: FromRequestUnmarshaller, In <: HList] =
    serveAdd[ReqBody[name, x], In, x, name] {
      entity(as[x])
    }

  implicit def headerServe[name <: Symbol: Witness.Aux, x: FromHeader, In <: HList] = serveAdd[Header[name, x], In, x, name] {
    headerValueByName(name[name]).flatMap(str => tryParse[x, FromHeader, name](str))
  }

  implicit def headerOptionServe[name <: Symbol: Witness.Aux, x: FromHeader, In <: HList] =
    serveAdd[Header[name, Option[x]], In, Option[x], name] {
      optionalHeaderValueByName(name[name]).flatMap {
        case Some(str) => tryParseOpt[x, FromHeader, name](str)
        case None      => provide(Option.empty[x])
      }
    }

  implicit def cookieServe[name <: Symbol: Witness.Aux, x: FromCookie, In <: HList] = serveAdd[Cookie[name, x], In, x, name] {
    cookie(name[name]).flatMap(cook => tryParse[x, FromCookie, name](cook.value))
  }

  implicit def cookieOptionServe[name <: Symbol: Witness.Aux, x: FromCookie, In <: HList] =
    serveAdd[Cookie[name, Option[x]], In, Option[x], name] {
      optionalCookie(name[name]).flatMap {
        case Some(cook) => tryParseOpt[x, FromCookie, name](cook.value)
        case None       => provide(Option.empty[x])
      }
    }

  implicit def formFieldServe[name <: Symbol: Witness.Aux, x: FromFormField, In <: HList] =
    serveAdd[FormField[name, x], In, x, name] {
      formField(name[name]).flatMap(str => tryParse[x, FromFormField, name](str))
    }

  implicit def formFieldOptionServe[name <: Symbol: Witness.Aux, x: FromFormField, In <: HList] =
    serveAdd[FormField[name, Option[x]], In, Option[x], name] {
      formFieldMap.flatMap { m =>
        m.get(name[name]) match {
          case Some(str) => tryParseOpt[x, FromFormField, name](str)
          case None      => provide(Option.empty[x])
        }
      }
    }

  implicit def basicAuthServe[realm: Name, name <: Symbol: Witness.Aux, x: BasicAuthenticator, In <: HList] =
    serveAdd[BasicAuth[realm, name, x], In, x, name] {
      BasicAuthenticator[x].directive(Name[realm].string)
    }

  implicit def bearerAuthServe[realm: Name, name <: Symbol: Witness.Aux, x: BearerAuthenticator, In <: HList] =
    serveAdd[BearerAuth[realm, name, x], In, x, name] {
      BearerAuthenticator[x].directive(Name[realm].string)
    }

  implicit def basicAuthOptServe[realm: Name, name <: Symbol: Witness.Aux, x: BasicAuthenticator, In <: HList] =
    serveAdd[BasicAuth[realm, name, Option[x]], In, Option[x], name] {
      BasicAuthenticator[x].directive(Name[realm].string).optional
    }

  implicit def bearerAuthOptServe[realm: Name, name <: Symbol: Witness.Aux, x: BearerAuthenticator, In <: HList] =
    serveAdd[BearerAuth[realm, name, Option[x]], In, Option[x], name] {
      BearerAuthenticator[x].directive(Name[realm].string).optional
    }

  implicit def apiKeyAuthServe[realm, Param <: CanHoldApiKey, In <: HList](
      implicit serve: Serve[Param, In]): Serve.Aux[ApiKeyAuth[realm, Param], In, serve.Out] =
    serve.as[ApiKeyAuth[realm, Param]]

  implicit def asServe[x, name, In <: HList, Head, old, Rest <: HList](
      implicit serveInner: Lazy[Serve.Aux[x, In, FieldType[old, Head] :: Rest]])
    : Serve.Aux[As[x, name], In, FieldType[name, Head] :: Rest] =
    serveInner.value.asInstanceOf[Serve.Aux[As[x, name], In, FieldType[name, Head] :: Rest]]

  implicit def metaServe[x <: Meta, In <: HList]: Aux[x, In, In] = serveCheck[x, In](pass)

  implicit def keyServe[name <: Symbol, In <: HList](implicit w: Witness.Aux[name]): Aux[Key[name], In, key[name] :: In] =
    new Serve[Key[name], In] {
      type Out = key[name] :: In
      def directive(in: In): Directive1[key[name] :: In] = provide(in).map(new key[name] :: _)
    }
}

final case class MethodCheck[T](method: HttpMethod)
object MethodCheck {
  implicit val checkGet: MethodCheck[Get]         = MethodCheck(HttpMethods.GET)
  implicit val checkPost: MethodCheck[Post]       = MethodCheck(HttpMethods.POST)
  implicit val checkDelete: MethodCheck[Delete]   = MethodCheck(HttpMethods.DELETE)
  implicit val checkPut: MethodCheck[Put]         = MethodCheck(HttpMethods.PUT)
  implicit val checkOptions: MethodCheck[Options] = MethodCheck(HttpMethods.OPTIONS)
  implicit val checkHead: MethodCheck[Head]       = MethodCheck(HttpMethods.HEAD)
  implicit val checkPatch: MethodCheck[Patch]     = MethodCheck(HttpMethods.PATCH)
}
trait ServeInstances1 { self: Serve.type =>

  implicit def queryParamsServe[name <: Symbol: Witness.Aux, x: FromQueryParam, In <: HList] =
    serveAdd[QueryParams[name, x], In, List[x], name] {
      parameterMultiMap.flatMap(
        _.get(name[name]).fold(Directive[Tuple1[List[x]]](_ => reject(MissingQueryParamRejection(name[name]))))(
          _.traverse[Directive1, x](value => tryParse[x, FromQueryParam, name](value)))
      )
    }
}
