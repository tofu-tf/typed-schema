package ru.tinkoff.tschema.named

import akka.http.scaladsl.server._
import Directives._
import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import ru.tinkoff.tschema.Name
import ru.tinkoff.tschema.serve._
import ru.tinkoff.tschema.typeDSL._
import shapeless.ops.hlist._
import shapeless.{::, HList, HNil, Witness, labelled}

trait ServePrefix[T, I <: HList] extends ServePartial[T, I, Nothing] {
  type Tag
  def handle(f: (I) ⇒ Route): Route
}

trait ServePrefixN[T, I <: HList] extends ServePrefix[T, I] {
  type Tag = Nothing
  def handle(f: (I) ⇒ Route): Route
}

object ServePrefix {
  private type Fld[name, x] = labelled.FieldType[name, x] :: HNil
  private def fld[name, x](x: x): Fld[name, x] = labelled.field[name](x) :: HNil

  private def prefixServe[pref, T](implicit name: Name[pref]) = new ServePrefixN[T, HNil] {
    def handle(f: (HNil) => Route): Route = pathPrefix(name.string)(f(HNil))
  }

  implicit def prefixWrapServe[pref: Name] = prefixServe[pref, Prefix[pref]]

  implicit def prefixStrServe[pref: Name] = prefixServe[pref, pref]

  implicit def prefixWitnessServe[pref: Name] = prefixServe[pref, Witness.Aux[pref]]

  implicit def queryParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new ServePrefixN[QueryParam[name, x], Fld[name, x]] {
    def handle(f: (Fld[name, x]) ⇒ Route): Route = parameter(name.symbol)(param ⇒ f(fld(fromQueryParam(param))))
  }

  implicit def queryOptParamServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new ServePrefixN[QueryParam[name, Option[x]], Fld[name, Option[x]]] {
    def handle(f: (Fld[name, Option[x]]) ⇒ Route): Route = parameter(name.symbol.?)(param ⇒ f(fld(param.map(fromQueryParam(_)))))
  }

  implicit def queryParamsServe[name, x]
  (implicit fromQueryParam: FromQueryParam[x], name: Name[name]) = new ServePrefixN[QueryParams[name, x], Fld[name, List[x]]] {
    def handle(f: (Fld[name, List[x]]) => Route): Route =
      parameterMultiMap(paramMap ⇒ f(fld(paramMap.getOrElse(name.string, Nil).map(fromQueryParam(_)))))
  }

  implicit def queryFlagServe[name, x]
  (implicit name: Name[name]) = new ServePrefixN[QueryFlag[name], Fld[name, Boolean]] {
    def handle(f: Fld[name, Boolean] => Route): Route =
      parameterMap(paramMap ⇒ f(fld(paramMap contains name.string)))
  }

  implicit def captureServe[name: Name, x]
  (implicit fromPathParam: FromPathParam[x]) = new ServePrefixN[Capture[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = pathPrefix(fromPathParam.matcher)(x ⇒ f(fld(x)))
  }

  implicit def reqBodyServe[x: FromRequestUnmarshaller] = new ServePrefixN[ReqBody[x], Fld[body, x]] {
    def handle(f: Fld[body, x] => Route): Route = entity(as[x])((x: x) ⇒ f(fld(x)))
  }

  implicit def headerServe[name: Name, x]
  (implicit fromHeader: FromHeader[x]) = new ServePrefixN[Header[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = headerValue(fromHeader(_))(x ⇒ f(fld(x)))
  }

  implicit def formFieldServe[name, x]
  (implicit fromFieldParam: FromFormField[x], name: Name[name]) = new ServePrefixN[FormField[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = formField(name.symbol)(field ⇒ f(fld(fromFieldParam(field))))
  }

  implicit def cookieServe[name, x]
  (implicit fromCookie: FromCookie[x], name: Name[name]) = new ServePrefixN[Cookie[name, x], Fld[name, x]] {
    def handle(f: Fld[name, x] => Route): Route = cookie(name.string)(cook ⇒ f(fld(fromCookie(cook.value))))
  }

  implicit def consServe[start, end, I1 <: HList, I2 <: HList, A, B]
  (implicit start: ServePrefix[start, I1] {type Tag = A},
   end: ServePrefix[end, I2] {type Tag = B},
   prepend: Prepend[I1, I2],
   chooseTag: ChooseTag[A, B]) =
    new ServePrefix[start :> end, prepend.Out] {
      type Tag = chooseTag.Out
      def handle(f: (prepend.Out) => Route): Route = start.handle(i1 ⇒ end.handle(i2 ⇒ f(prepend(i1, i2))))
    }

  implicit def metaServe[x <: Meta] = new ServePrefixN[x, HNil] {
    def handle(f: (HNil) => Route): Route = f(HNil)
  }
}







