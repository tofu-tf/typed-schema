package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import ru.tinkoff.tschema._
import ru.tinkoff.tschema.typeDSL.{Capture, Cookie, FormField, Header, Meta, Prefix, QueryFlag, QueryParam, QueryParams, ReqBody}
import shapeless._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._

trait ServeElement[T] {
  type Input
  type Output = Nothing
  def apply(f: (Input) ⇒ Route): Route
}

object ServeElement {
  type Aux[T, I] = ServeElement[T] {type Input = I}

  abstract class Element0[T] extends ServeElement[T] {type Input = HNil}
  abstract class Element1[T, A] extends ServeElement[T] {type Input = A :: HNil}
  import labelled.FieldType
  private type Fld[name, x] = FieldType[name, x] :: HNil

  private def fld[name, x](x: x): FieldType[name, x] :: HNil = labelled.field[name](x) :: HNil

  type body = Witness.`'body`.T

  private def prefixServe[pref, T](implicit name: Name[pref]) = new Element0[T] {
    def apply(f: (HNil) => Route): Route = pathPrefix(name.string)(f(HNil))
  }

  private def tryParse[T, name, F[x] <: FromParam[x]](value: String)(f: T ⇒ Route)(implicit parse: F[T], name: Name[name]): Route =
    parse(value) match {
      case Right(result) ⇒ f(result)
      case Left(err) ⇒ reject(ParamFormatRejection(name.string, err))
    }

  implicit def prefixWrapServe[pref: Name] = prefixServe[pref, Prefix[pref]]

  implicit def prefixStrServe[pref: Name] = prefixServe[pref, pref]

  implicit def prefixWitnessServe[pref: Name] = prefixServe[pref, Witness.Aux[pref]]

  implicit def queryParamServe[name, x: FromQueryParam](implicit name: Name[name]) = new Element1[QueryParam[name, x], FieldType[name, x]] {
    def apply(f: (FieldType[name, x]) :: HNil ⇒ Route): Route =
      parameter(name.symbol)(param ⇒ tryParse[x, name, FromQueryParam](param)(value ⇒ f(fld(value))))
  }

  implicit def queryOptParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new Element1[QueryParam[name, Option[x]], FieldType[name, Option[x]]] {
    def apply(f: (Fld[name, Option[x]]) ⇒ Route): Route =
      parameter(name.symbol.?)(param ⇒ f(fld(param.flatMap(x ⇒ fromQueryParam(x).toOption))))
  }

  implicit def queryParamsServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new Element1[QueryParams[name, x], FieldType[name, List[x]]] {
    def apply(f: (Fld[name, List[x]]) => Route): Route =
      parameterMultiMap(paramMap ⇒ paramMap.getOrElse(name.string, Nil).traverse(fromQueryParam(_)) match {
        case Left(err) ⇒ reject(ParamFormatRejection(name.string, err))
        case Right(lst) ⇒ f(fld(lst))
      })
  }

  implicit def queryFlagServe[name, x]
  (implicit name: Name[name]) = new Element1[QueryFlag[name], FieldType[name, Boolean]] {
    def apply(f: Fld[name, Boolean] => Route): Route =
      parameterMap(paramMap ⇒ f(fld(paramMap contains name.string)))
  }

  implicit def captureServe[name: Name, x]
  (implicit fromPathParam: FromPathParam[x]) = new Element1[Capture[name, x], FieldType[name, x]] {
    def apply(f: Fld[name, x] => Route): Route = pathPrefix(fromPathParam.matcher)(x ⇒ f(fld(x)))
  }

  implicit def reqBodyServe[x: FromRequestUnmarshaller] = new Element1[ReqBody[x], FieldType[body, x]] {
    def apply(f: Fld[body, x] => Route): Route = entity(as[x])((x: x) ⇒ f(fld(x)))
  }

  implicit def headerServe[name, x: FromHeader]
  (implicit name: Name[name]) = new Element1[Header[name, x], FieldType[name, x]] {
    def apply(f: Fld[name, x] => Route): Route =
      headerValueByName(name.string)(str ⇒ tryParse[x, name, FromHeader](str)(x ⇒ f(fld(x))))
  }

  implicit def formFieldServe[name, x: FromFormField]
  (implicit name: Name[name]) = new Element1[FormField[name, x], FieldType[name, x]] {
    def apply(f: Fld[name, x] => Route): Route =
      formField(name.symbol)(field ⇒ tryParse[x, name, FromFormField](field)(value ⇒ f(fld(value))))
  }

  implicit def cookieServe[name, x: FromCookie]
  (implicit name: Name[name]) = new Element1[Cookie[name, x], FieldType[name, x]] {
    def apply(f: Fld[name, x] => Route): Route =
      cookie(name.string)(cook ⇒ tryParse[x, name, FromCookie](cook.value)(value => f(fld(value))))
  }

  implicit def metaServe[x <: Meta] = new Element0[x] {
    def apply(f: (HNil) => Route): Route = f(HNil)
  }
}

