package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import Directives._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import ru.tinkoff.tschema.Name
import ru.tinkoff.tschema.serve._
import ru.tinkoff.tschema.typeDSL._
import shapeless.labelled
import shapeless.ops.hlist._
import shapeless.{::, HList, HNil, Witness}

trait ServeNamedPrefix[T, I <: HList] {
  def handle(f: (I) ⇒ Route): Route
}

object ServeNamedPrefix {
  private type Fld[name, x] = labelled.FieldType[name, x] :: HNil
  private def fld[name, x](x: x): Fld[name, x] = labelled.field[name](x) :: HNil

  private def prefixServe[pref, T](implicit name: Name[pref]) = new ServeNamedPrefix[T, HNil] {
    def handle(f: (HNil) => Route): Route = pathPrefix(name.string)(f(HNil))
  }

  implicit def prefixWrapServe[pref: Name] = prefixServe[pref, Prefix[pref]]

  implicit def prefixStrServe[pref: Name] = prefixServe[pref, pref]

  implicit def prefixWitnessServe[pref: Name] = prefixServe[pref, Witness.Aux[pref]]

  implicit def queryParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new ServeNamedPrefix[QueryParam[name, x], Fld[name, x]] {
    def handle(f: (Fld[name, x]) ⇒ Route): Route = parameter(name.symbol)(param ⇒ f(fld(fromQueryParam(param))))
  }

  implicit def queryOptParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new ServeNamedPrefix[QueryParam[name, Option[x]], Fld[name, Option[x]]] {
    def handle(f: (Fld[name, Option[x]]) ⇒ Route): Route = parameter(name.symbol.?)(param ⇒ f(fld(param.map(fromQueryParam(_)))))
  }

  implicit def queryParamsServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new ServeNamedPrefix[QueryParams[name, x], Fld[name, List[x]]] {
    def handle(f: (Fld[name, List[x]]) => Route): Route =
      parameterMultiMap(paramMap ⇒ f(fld(paramMap.getOrElse(name.string, Nil).map(fromQueryParam(_)))))
  }

  implicit def queryFlagServe[name, x]
  (implicit name: Name[name]) = new ServeNamedPrefix[QueryFlag[name], Fld[name, Boolean]] {
    def handle(f: Fld[name, Boolean] => Route): Route =
      parameterMap(paramMap ⇒ f(fld(paramMap contains name.string)))
  }

  implicit def captureServe[name: Name, x]
  (implicit fromPathParam: FromPathParam[x]) = new ServeNamedPrefix[Capture[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = pathPrefix(fromPathParam.matcher)(x ⇒ f(fld(x)))
  }

  implicit def reqBodyServe[x: FromRequestUnmarshaller] = new ServeNamedPrefix[ReqBody[x], Fld[body, x]] {
    def handle(f: Fld[body, x] => Route): Route = entity(as[x])((x: x) ⇒ f(fld(x)))
  }

  implicit def headerServe[name: Name, x]
  (implicit fromHeader: FromHeader[x]) = new ServeNamedPrefix[Header[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = headerValue(fromHeader(_))(x ⇒ f(fld(x)))
  }

  implicit def formFieldServe[name, x]
  (implicit fromFieldParam: FromFormField[x], name: Name[name]) = new ServeNamedPrefix[FormField[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = formField(name.symbol)(field ⇒ f(fld(fromFieldParam(field))))
  }

  implicit def cookieServe[name, x]
  (implicit fromCookie: FromCookie[x], name: Name[name]) = new ServeNamedPrefix[Cookie[name, x], Fld[name, x ]] {
    def handle(f: Fld[name, x] => Route): Route = cookie(name.string)(cook ⇒ f(fld(fromCookie(cook.value))))
  }

  implicit def consServe[start, end, I1 <: HList, I2 <: HList]
  (implicit start: ServeNamedPrefix[start, I1], end: ServeNamedPrefix[end, I2], prepend: Prepend[I1, I2]) =
    new ServeNamedPrefix[start :> end, prepend.Out] {
      def handle(f: (prepend.Out) => Route): Route = start.handle(i1 ⇒ end.handle(i2 ⇒ f(prepend(i1, i2))))
    }

  implicit def metaServe[x <: Meta] = new ServeNamedPrefix[x, HNil] {
    def handle(f: (HNil) => Route): Route = f(HNil)
  }
}





