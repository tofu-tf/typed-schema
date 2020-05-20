package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.model.{HttpMethod, HttpMethods}
import akka.http.scaladsl.server
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import cats.data.OptionT
import ru.tinkoff.tschema._
import cats.syntax.either._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{Segment => _, _}
import shapeless.labelled.{FieldType, field}
import catsInstances._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.option._
import cats.instances.either._
import ru.tinkoff.tschema.akkaHttp.auth.{BasicAuthenticator, BearerAuthenticator}
import ru.tinkoff.tschema.param.ParamSource.All
import ru.tinkoff.tschema.param.{
  MissingParamError,
  MultiParam,
  MultiParamError,
  Param,
  ParamError,
  ParamSource,
  ParseParamError,
  SingleParam,
  SingleParamError
}
import ru.tinkoff.tschema.common.Name
import shapeless.ops.record.Selector

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}

@implicitNotFound("could not serve ${T} using input ${In} ")
trait Serve[T, In <: HList] {
  type Out <: HList
  def directive(in: In): Directive1[Out]

  def as[Q]: Serve.Aux[Q, In, Out] = asInstanceOf[Serve.Aux[Q, In, Out]]
}

object Serve extends ServeInstances {
  def nil[T](implicit serve: Serve[T, HNil]): Aux[T, HNil, serve.Out]         = serve
  def apply[L <: HList, T](implicit serve: Serve[T, L]): Aux[T, L, serve.Out] = serve
}

private[akkaHttp] trait ServeTypes {
  type Aux[T, In <: HList, O <: HList] = Serve[T, In] { type Out = O }

  type Check[T, In <: HList]           = Serve[T, In] { type Out = In }
  type Add[T, In <: HList, key, value] = Serve[T, In] { type Out = FieldType[key, value] :: In }
  type Push[T, In <: HList, value]     = Serve[T, In] { type Out = value :: In }
}

private[akkaHttp] trait ServeFunctions extends ServeTypes {
  protected def resolveParam[S >: All <: ParamSource, name, A](implicit
      param: Param[S, A],
      w: Name[name],
      directives: ParamDirectives[S]
  ): Directive1[A] = param match {
    case single: SingleParam[S, A] =>
      directives.getByName(w.string).flatMap(s => directives.provideOrReject(w.string, single.applyOpt(s)))
    case multi: MultiParam[S, A]   =>
      multi.names.traverse(directives.getByName).flatMap(ls => directives.provideOrReject(w.string, multi.applyOpt(ls)))
  }

  def serveAdd[T, In <: HList, A, key](dir: Directive1[A]): Add[T, In, key, A] = new Serve[T, In] {
    type Out = FieldType[key, A] :: In
    def directive(in: In): Directive1[FieldType[key, A] :: In] = dir.map(field[key](_) :: in)
  }

  def servePush[T, In <: HList, A](dir: Directive1[A]): Push[T, In, A] = new Serve[T, In] {
    type Out = A :: In
    def directive(in: In): Directive1[A :: In] = dir.map(_ :: in)
  }

  def serveReadAdd[T, param, Value, In <: HList, A, key](
      dir: Value => Directive1[A]
  )(implicit read: Selector.Aux[In, param, Value]): Add[T, In, key, A] = new Serve[T, In] {
    type Out = FieldType[key, A] :: In
    def directive(in: In): Directive1[FieldType[key, A] :: In] = dir(read(in)).map(field[key](_) :: in)
  }

  def serveAddIn[T, In <: HList, A, key](f: In => Directive1[A]): Add[T, In, key, A] = new Serve[T, In] {
    type Out = FieldType[key, A] :: In
    def directive(in: In): Directive1[FieldType[key, A] :: In] = f(in).map(field[key](_) :: in)
  }

  def serveCheck[T, In <: HList](dir: Directive0): Check[T, In] = new Serve[T, In] {
    type Out = In
    def directive(in: In): Directive1[In] = dir.tmap(_ => in)
  }

  def serveReadCheck[T, param, Value, In <: HList](
      dir: Value => Directive0
  )(implicit read: Selector.Aux[In, param, Value]): Check[T, In] =
    new Serve[T, In] {
      override type Out = In
      override def directive(in: In): Directive1[In] = dir(read(in)).tmap(_ => in)
    }

  def serveCheckIn[T, In <: HList](f: In => Directive0): Check[T, In] = new Serve[T, In] {
    override type Out = In
    override def directive(in: In): Directive1[In] = f(in).tmap(_ => in)
  }

  def serveMap[T, In <: HList, nameA, nameB, A, B](
      f: A => B
  )(implicit select: Selector.Aux[In, nameA, A]): Aux[T, In, FieldType[nameB, B] :: In] = new Serve[T, In] {
    type Out = FieldType[nameB, B] :: In
    def directive(in: In): Directive1[Out] = provide(field[nameB](f(select(in))) :: in)
  }

  def serveMap2[T, In <: HList, nameA, nameB, nameC, A, B, C](f: (A, B) => C)(implicit
      selectA: Selector.Aux[In, nameA, A],
      selectB: Selector.Aux[In, nameB, B]
  ): Aux[T, In, FieldType[nameC, C] :: In] = new Serve[T, In] {
    type Out = FieldType[nameC, C] :: In
    def directive(in: In): Directive1[Out] = provide(field[nameC](f(selectA(in), selectB(in))) :: in)
  }

  def serveFMap[T, In <: HList, nameA, nameB, A, B](
      f: A => Future[B]
  )(implicit select: Selector.Aux[In, nameA, A], ec: ExecutionContext): Aux[T, In, FieldType[nameB, B] :: In] =
    new Serve[T, In] {
      type Out = FieldType[nameB, B] :: In
      def directive(in: In): Directive1[Out] = Directive { handle => ctx =>
        for {
          b   <- f(select(in): A)
          out  = field[nameB](b) :: in
          res <- handle(Tuple1(out))(ctx)
        } yield res
      }
    }

  def serveFilter[T, In <: HList, name, A](
      f: A => Option[Rejection]
  )(implicit select: Selector.Aux[In, name, A]): Aux[T, In, In] = new Serve[T, In] {
    type Out = In
    def directive(in: In): Directive1[In] = Directive { handle =>
      f(select(in)) match {
        case Some(rej) => reject(rej)
        case None      => handle(Tuple1(in))
      }
    }
  }

  def identity[T, In <: HList]: Check[T, In] = serveCheck(Directive.Empty)
}

private[akkaHttp] trait ServeInstances extends ServeFunctions with ServeInstances1 with ServeAuthInstances {
  self: Serve.type =>
  implicit def prefixServe[pref, In <: HList](implicit n: Name[pref]) =
    serveCheck[Prefix[pref], In](pathPrefix(n.string))

  implicit def methodServe[method, In <: HList](implicit check: MethodCheck[method]) =
    serveCheck[method, In](method(check.method))

  implicit def queryMap[name: Name, x, In <: HList] =
    serveAdd[AllQuery[name], In, Map[String, String], name](parameterMap)

  implicit def queryParamServe[name: Name, x: Param.PQuery, In <: HList] =
    serveAdd[QueryParam[name, x], In, x, name](resolveParam[ParamSource.Query, name, x])

  implicit def queryFlagServe[name: Name, x, In <: HList] = serveAdd[QueryFlag[name], In, Boolean, name](
    parameterMap.map(_.contains(Name[name].string))
  )

  implicit def captureServe[name: Name, x: Param.PPath, In <: HList] =
    serveAdd[Capture[name, x], In, x, name](resolveParam[ParamSource.Path, name, x])

  implicit def reqBodyServe[name: Name, x: FromRequestUnmarshaller, In <: HList] =
    serveAdd[ReqBody[name, x], In, x, name] {
      entity(as[x])
    }

  implicit def headerServe[name: Name, x: Param.PHeader, In <: HList] =
    serveAdd[Header[name, x], In, x, name](resolveParam[ParamSource.Header, name, x])

  implicit def cookieServe[name: Name, x: Param.PCookie, In <: HList] =
    serveAdd[Cookie[name, x], In, x, name](resolveParam[ParamSource.Cookie, name, x])

  implicit def formFieldServe[name: Name, x: Param.PForm, In <: HList] =
    serveAdd[FormField[name, x], In, x, name](resolveParam[ParamSource.Form, name, x])

  implicit def asServe[x, name, In <: HList, Head, old]
      : Serve.Aux[As[name], FieldType[old, Head] :: In, FieldType[name, Head] :: In] =
    Serve.identity.asInstanceOf[Serve.Aux[As[name], FieldType[old, Head] :: In, FieldType[name, Head] :: In]]

  implicit def metaServe[x <: Meta, In <: HList]: Aux[x, In, In] = serveCheck[x, In](pass)

  implicit def keyServe[name, In <: HList]: Push[Key[name], In, Key[name]] =
    servePush(provide(Key.of[name]))

  implicit def groupServe[name, In <: HList]: Push[Group[name], In, Group[name]] =
    servePush(provide(Group.of[name]))
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

private[akkaHttp] trait ServeInstances1 { self: Serve.type =>

  implicit def queryParamsServe[name: Name, x, In <: HList](implicit param: SingleParam[ParamSource.Query, x]) =
    serveAdd[QueryParams[name, x], In, List[x], name](extractQueryParams[name, x](allowEmpty = false))

  implicit def queryOptParamsServe[name: Name, x, In <: HList](implicit param: SingleParam[ParamSource.Query, x]) =
    serveAdd[QueryParams[name, Option[x]], In, List[x], name](extractQueryParams[name, x](allowEmpty = true))

  private def extractQueryParams[name: Name, x](
      allowEmpty: Boolean
  )(implicit param: SingleParam[ParamSource.Query, x]): Directive1[List[x]] = {
    val name = Name[name].string

    def rejection(err: SingleParamError): Route =
      reject(err match {
        case MissingParamError       => MissingQueryParamRejection(name)
        case ParseParamError(errTxt) => MalformedQueryParamRejection(name, errTxt)
      })

    parameterMultiMap.flatMap { multiMap =>
      Directive { f =>
        multiMap
          .get(name)
          .fold(if (allowEmpty) Right(Nil) else param.applyOpt(None).map(List(_)))(
            _.traverse(x => param.applyOpt(Some(x)))
          )
          .fold(rejection, x => f(Tuple1(x)))
      }
    }
  }
}

private[akkaHttp] trait ServeAuthInstances extends ServeFunctions {
  implicit def basicAuthServe[realm: Name, name, x: BasicAuthenticator, In <: HList] =
    serveAdd[BasicAuth[realm, name, x], In, x, name] {
      BasicAuthenticator[x].directive(Name[realm].string)
    }

  implicit def bearerAuthServe[realm: Name, name, x: BearerAuthenticator, In <: HList] =
    serveAdd[BearerAuth[realm, name, x], In, x, name] {
      BearerAuthenticator[x].directive(Name[realm].string)
    }

  implicit def basicAuthOptServe[realm: Name, name, x: BasicAuthenticator, In <: HList] =
    serveAdd[BasicAuth[realm, name, Option[x]], In, Option[x], name] {
      BasicAuthenticator[x].directive(Name[realm].string).optional
    }

  implicit def bearerAuthOptServe[realm: Name, name, x: BearerAuthenticator, In <: HList] =
    serveAdd[BearerAuth[realm, name, Option[x]], In, Option[x], name] {
      BearerAuthenticator[x].directive(Name[realm].string).optional
    }

  implicit def apiKeyAuthServe[realm, Param <: CanHoldApiKey, In <: HList](implicit
      serve: Serve[Param, In]
  ): Serve.Aux[ApiKeyAuth[realm, Param], In, serve.Out] =
    serve.as[ApiKeyAuth[realm, Param]]
}

trait ParamDirectives[S <: ParamSource] {
  def getByName(name: String): Directive1[Option[String]]
  def notFound(name: String): Rejection
  def malformed(name: String, error: String): Rejection

  def singleRejection(name: String, error: SingleParamError): Rejection =
    error match {
      case MissingParamError    => notFound(name)
      case ParseParamError(err) => malformed(name, err)
    }

  def errorReject[A](name: String, error: ParamError): Directive1[A] =
    error match {
      case single: SingleParamError => reject(singleRejection(name, single))
      case MultiParamError(vals)    =>
        reject(vals.map { case (field, err) => singleRejection(field, err) }.toSeq: _*)
    }

  def provideOrReject[A](name: String, result: Param.Result[A]): Directive1[A] =
    result.fold(errorReject[A](name, _), provide)
}

object ParamDirectives {
  type TC[A <: ParamSource] = ParamDirectives[A]
  import ParamSource._
  implicit val queryParamDirective: TC[Query] = new TC[Query] {
    def getByName(name: String): Directive1[Option[String]] = parameter(name.?)
    def notFound(name: String): Rejection                   = MissingQueryParamRejection(name)
    def malformed(name: String, error: String): Rejection   = MalformedQueryParamRejection(name, error)
  }

  implicit val cookieParamDirectives: TC[Cookie] = new TC[Cookie] {
    def getByName(name: String): Directive1[Option[String]] = optionalCookie(name).map(_.map(_.value))
    def notFound(name: String): Rejection                   = MissingCookieRejection(name)
    def malformed(name: String, error: String): Rejection   = MalformedHeaderRejection(s"cookie: $name", error)
  }

  implicit val pathParamDirectives: TC[Path] = new TC[Path] {
    def getByName(name: String): Directive1[Option[String]] = pathPrefix(Segment).map(Some(_): Option[String])
    def notFound(name: String): Rejection                   = NotFoundPathRejection(name)
    def malformed(name: String, error: String): Rejection   = MalformedPathRejection(name, error)
  }

  implicit val formDataParamDirectives: TC[Form] = new TC[Form] {
    def getByName(name: String): Directive1[Option[String]] = formField(name.?)
    def notFound(name: String): Rejection                   = MissingFormFieldRejection(name)
    def malformed(name: String, error: String): Rejection   = MalformedFormFieldRejection(name, error)
  }

  implicit val headerParamDirectives: TC[Header] = new TC[Header] {
    def getByName(name: String): Directive1[Option[String]] = optionalHeaderValueByName(name)
    def notFound(name: String): Rejection                   = MissingHeaderRejection(name)
    def malformed(name: String, error: String): Rejection   = MalformedHeaderRejection(name, error)
  }

}
