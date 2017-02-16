package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import ru.tinkoff.tschema.serve._
import ru.tinkoff.tschema.typeDSL.{Capture, Cookie, FormField, Header, Meta, Prefix, QueryFlag, QueryParam, QueryParams, ReqBody}
import shapeless._

trait ServeElement[T, I <: HList] extends ServePartial[T, I, Nothing]{
  def handle(f: (I) ⇒ Route): Route
}

object ServeElement {
  type Element0[T] = ServeElement[T, HNil]
  type Element1[T, A] = ServeElement[T, A :: HNil]
  import labelled.FieldType
  private type Fld[name, x] = FieldType[name, x] :: HNil

  private def fld[name, x](x: x): FieldType[name, x] :: HNil = labelled.field[name](x) :: HNil

  type body = Witness.`'body`.T

  private def prefixServe[pref, T](implicit name: Name[pref]) = new Element0[T] {
    def handle(f: (HNil) => Route): Route = pathPrefix(name.string)(f(HNil))
  }

  implicit def prefixWrapServe[pref: Name] = prefixServe[pref, Prefix[pref]]

  implicit def prefixStrServe[pref: Name] = prefixServe[pref, pref]

  implicit def prefixWitnessServe[pref: Name] = prefixServe[pref, Witness.Aux[pref]]

  implicit def queryParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new Element1[QueryParam[name, x], FieldType[name, x]] {
    def handle(f: (FieldType[name, x]) :: HNil ⇒ Route): Route = parameter(name.symbol)(param ⇒ f(fld(fromQueryParam(param))))
  }

  implicit def queryOptParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new Element1[QueryParam[name, Option[x]], FieldType[name, Option[x]]] {
    def handle(f: (Fld[name, Option[x]]) ⇒ Route): Route = parameter(name.symbol.?)(param ⇒ f(fld(param.map(fromQueryParam(_)))))
  }

  implicit def queryParamsServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new Element1[QueryParams[name, x], FieldType[name, List[x]]] {
    def handle(f: (Fld[name, List[x]]) => Route): Route =
      parameterMultiMap(paramMap ⇒ f(fld(paramMap.getOrElse(name.string, Nil).map(fromQueryParam(_)))))
  }

  implicit def queryFlagServe[name, x]
  (implicit name: Name[name]) = new Element1[QueryFlag[name], FieldType[name, Boolean]] {
    def handle(f: Fld[name, Boolean] => Route): Route =
      parameterMap(paramMap ⇒ f(fld(paramMap contains name.string)))
  }

  implicit def captureServe[name: Name, x]
  (implicit fromPathParam: FromPathParam[x]) = new Element1[Capture[name, x], FieldType[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = pathPrefix(fromPathParam.matcher)(x ⇒ f(fld(x)))
  }

  implicit def reqBodyServe[x: FromRequestUnmarshaller] = new Element1[ReqBody[x], FieldType[body, x]] {
    def handle(f: Fld[body, x] => Route): Route = entity(as[x])((x: x) ⇒ f(fld(x)))
  }

  implicit def headerServe[name: Name, x]
  (implicit fromHeader: FromHeader[x]) = new Element1[Header[name, x], FieldType[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = headerValue(fromHeader(_))(x ⇒ f(fld(x)))
  }

  implicit def formFieldServe[name, x]
  (implicit fromFieldParam: FromFormField[x], name: Name[name]) = new Element1[FormField[name, x], FieldType[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = formField(name.symbol)(field ⇒ f(fld(fromFieldParam(field))))
  }

  implicit def cookieServe[name, x]
  (implicit fromCookie: FromCookie[x], name: Name[name]) = new Element1[Cookie[name, x], FieldType[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = cookie(name.string)(cook ⇒ f(fld(fromCookie(cook.value))))
  }

  implicit def metaServe[x <: Meta] = new Element0[x] {
    def handle(f: (HNil) => Route): Route = f(HNil)
  }
}

