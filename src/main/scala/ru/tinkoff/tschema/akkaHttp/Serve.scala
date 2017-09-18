package ru.tinkoff.tschema.akkaHttp
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
import shapeless.ops.hlist.Selector

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}

@implicitNotFound("could not serve ${T} using input ${In}")
trait Serve[T, In <: HList] {
  type Out <: HList
  def directive(in: In): Directive1[Out]
}

object Serve extends ServeInstances {
}

private[akkaHttp] trait ServeTypes {
  type body = Witness.`'body`.T
  type Aux[T, In <: HList, O <: HList] = Serve[T, In] {type Out = O}
  class key[name] protected[akkaHttp]()
}

private[akkaHttp] trait ServeFunctions extends ServeTypes {
  protected def tryParse[T, F[x] <: FromParam[x], name <: Symbol](value: String)(implicit parse: F[T], w: Witness.Aux[name]): Directive1[T] =
    Directive[Tuple1[T]](f => parse(value) match {
      case Right(result) => f(Tuple1(result))
      case Left(err) => reject(ParamFormatRejection(w.value.name, err))
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

  def serveMap[T, In <: HList, nameA, nameB, A, B]
  (f: A => B)
  (implicit select: Selector[In, FieldType[nameA, A]]): Aux[T, In, FieldType[nameB, B] :: In] = new Serve[T, In] {
    type Out = FieldType[nameB, B] :: In
    def directive(in: In): Directive1[Out] = provide(field[nameB](f(select(in))) :: in)
  }

  def serveMap2[T, In <: HList, nameA, nameB, nameC, A, B, C]
  (f: (A, B) => C)
  (implicit selectA: Selector[In, FieldType[nameA, A]],
   selectB: Selector[In, FieldType[nameB, B]]): Aux[T, In, FieldType[nameC, C] :: In] = new Serve[T, In] {
    type Out = FieldType[nameC, C] :: In
    def directive(in: In): Directive1[Out] = provide(field[nameC](f(selectA(in), selectB(in))) :: in)
  }

  def serveFMap[T, In <: HList, nameA, nameB, A, B]
  (f: A => Future[B])
  (implicit select: Selector[In, FieldType[nameA, A]],
   ec: ExecutionContext): Aux[T, In, FieldType[nameB, B] :: In] = new Serve[T, In] {
    type Out = FieldType[nameB, B] :: In
    def directive(in: In): Directive1[Out] = Directive { handle =>
      ctx =>
        for {
          b <- f(select(in): A)
          out = field[nameB](b) :: in
          res <- handle(Tuple1(out))(ctx)
        } yield res
    }
  }

  def serveFilter[T, In <: HList, name, A]
  (f: A => Option[Rejection])
  (implicit select: Selector[In, FieldType[name, A]]): Aux[T, In, In] = new Serve[T, In] {
    type Out = In
    def directive(in: In): Directive1[In] = Directive { handle =>
      f(select(in)) match {
        case Some(rej) => reject(rej)
        case None => handle(Tuple1(in))
      }
    }
  }
}

private[akkaHttp] trait ServeInstances extends ServeFunctions {
  implicit def prefixServe[pref <: Symbol, In <: HList](implicit w: Witness.Aux[pref]) = serveCheck[Prefix[pref], In](pathPrefix(w.value.name))

  implicit def queryParamServe[name <: Symbol : Witness.Aux, x: FromQueryParam, In <: HList] = serveAdd[QueryParam[name, x], In, x, name](
    parameter(name[name]).flatMap(param => tryParse[x, FromQueryParam, name](param))
  )

  implicit def queryOptParamServe[name <: Symbol : Witness.Aux, x: FromQueryParam, In <: HList] = serveAdd[QueryParam[name, Option[x]], In, Option[x], name](
    OptionT[Directive1, String](parameter(name[name].?)).flatMap { param => OptionT.liftF(tryParse[x, FromQueryParam, name](param)) }.value
  )

  implicit def queryParamsServe[name <: Symbol : Witness.Aux, x: FromQueryParam, In <: HList] = serveAdd[QueryParams[name, x], In, List[x], name] {
    parameterMultiMap.flatMap(_.getOrElse(name[name], Nil).traverse[Directive1, x](value => tryParse[x, FromQueryParam, name](value)))
  }

  implicit def queryFlagServe[name <: Symbol : Witness.Aux, x, In <: HList] = serveAdd[QueryFlag[name], In, Boolean, name](
    parameterMap.map(_.contains(name[name]))
  )

  implicit def captureServe[name: Witness.Aux, x, In <: HList](implicit fromPathParam: FromPathParam[x]) = serveAdd[Capture[name, x], In, x, name] {
    pathPrefix(fromPathParam.matcher)
  }

  implicit def reqBodyServe[x: FromRequestUnmarshaller, In <: HList] = serveAdd[ReqBody[x], In, x, body] {
    entity(as[x])
  }

  implicit def headerServe[name <: Symbol : Witness.Aux, x: FromHeader, In <: HList] = serveAdd[Header[name, x], In, x, name] {
    headerValueByName(name[name]).flatMap(str => tryParse[x, FromHeader, name](str))
  }

  implicit def cookieServe[name <: Symbol : Witness.Aux, x: FromCookie, In <: HList] = serveAdd[Cookie[name, x], In, x, name] {
    cookie(name[name]).flatMap(cook => tryParse[x, FromCookie, name](cook.value))
  }

  implicit def formFieldServe[name <: Symbol : Witness.Aux, x: FromFormField, In <: HList] = serveAdd[FormField[name, x], In, x, name] {
    formField(name[name]).flatMap(str => tryParse[x, FromFormField, name](str))
  }

  implicit def metaServe[x <: Meta, In <: HList]: Aux[x, In, In] = serveCheck[x, In](pass)

  implicit def keyServe[name <: Symbol, In <: HList](implicit w: Witness.Aux[name]): Aux[Key[name], In, key[name] :: In] = new Serve[Key[name], In] {
    type Out = key[name] :: In
    def directive(in: In): Directive1[key[name] :: In] = provide(in).map(new key[name] :: _)
  }
}


