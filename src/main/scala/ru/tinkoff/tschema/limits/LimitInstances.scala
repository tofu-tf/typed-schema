package ru.tinkoff.tschema.limits
import akka.http.scaladsl.server.RouteResult.Rejected
import akka.http.scaladsl.server.{Route, RouteResult}
import ru.tinkoff.tschema.limits.LimitHandler.{LimitRate, Pattern}
import ru.tinkoff.tschema.named.{Name, Provide, ServeMiddle}
import ru.tinkoff.tschema.swagger.{DerivedMkSwagger, DerivedMkSwaggerPrefix, MkSwagger}
import shapeless._
import shapeless.ops.hlist.{Reify, ToList}
import shapeless.ops.record._

import scala.concurrent.{Future, ExecutionContext ⇒ EC}

trait LimitInstances {
  implicit def limitMiddleware[P <: HList, l <: Limit[_, _], key]
  (implicit limitDef: LimitDef[l, key, P], ec: EC): ServeMiddle[l, P, key] =
    new ServeMiddle[l, P, key] {
      def apply(f: (P) ⇒ Route, provide: Provide[P]): Route = provide(
        p ⇒ ctx ⇒ limitDef.hasExceeded(p).flatMap {
          case None ⇒ f(p)(ctx)
          case Some(rejection) ⇒ Future.successful(Rejected(List(rejection)))
        }
      )
    }

  implicit def limitSwagger[l <: Limit[_, _]]: DerivedMkSwaggerPrefix[l] =
    DerivedMkSwaggerPrefix.fromFunc(identity)
}

trait LimitDef[l <: Limit[_, _], key, P <: HList] {
  type Params <: HList

  def periodMillis: Long
  def hasExceeded(params: P): Future[Option[LimitRejection]]
}

object LimitDef {
  type Aux[l <: Limit[_, _], key, P <: HList, Ps <: HList] = LimitDef[l, key, P] {type Params = Ps}

  implicit def fromHandler[count <: Int, unit <: TimeUnit, key, P <: HList, names <: HList, namesR <: HList]
  (implicit handler: LimitHandler,
   selectAll: SelectAll[P, names],
   count: Witness.Aux[count],
   unit: Witness.Aux[unit],
   key: Name[key],
   names: Reify.Aux[names, namesR],
   nameList: ToList[namesR, Symbol],
   ec: EC
  ): Aux[Limit[names, Rate[count, unit]], key, P, selectAll.Out] =
    new LimitDef[Limit[names, Rate[count, unit]], key, P] {
      type Params = selectAll.Out
      val rate = LimitRate(count.value, unit.value.millis)
      def periodMillis: Long = unit.value.millis
      def hasExceeded(params: P): Future[Option[LimitRejection]] =
        handler.check(Pattern(key.string, selectAll(params)), rate).map {
          case true ⇒ None
          case false ⇒ Some(LimitRejection(key.string, count.value, unit.value.toString, nameList(names()).map(_.name)))
        }
    }
}
