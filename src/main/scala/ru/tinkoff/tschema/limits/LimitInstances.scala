package ru.tinkoff.tschema.limits
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.RouteResult.Rejected
import ru.tinkoff.tschema.limits.LimitHandler.{LimitRate, Pattern}
import ru.tinkoff.tschema.akka2.{FindKey, Serve}
import ru.tinkoff.tschema.swagger.SwaggerMapper
import shapeless._
import shapeless.ops.hlist.{Reify, ToList}
import shapeless.ops.record.SelectAll
import shapeless.ops.hlist.Selector

import scala.concurrent.{Future, ExecutionContext => EC}

trait LimitInstances {
  implicit def limitMiddleware[In <: HList, Par <: HList]
  (implicit  findKey: Selector[In, Serve.key],
   selectParams: SelectAll[In, Par],
   limitDef: LimitDef[Par],
   ec: EC): Serve.Aux[Limit[Par], In, In] = new Serve[Limit[Par], In] {
    type Out = In
    def directive(in: In): Directive1[In] = Directive {
      val params = selectParams(in)
      f =>
        limitDef.directive(params, "asdasd") {
          f(Tuple1(in))
        }
    }
  }

  implicit def limitSwagger[l <: Limit[_]]: SwaggerMapper[l] = SwaggerMapper.empty[l]
}

trait LimitDef[Par <: HList] {
  def directive(params: HList, key: String): Directive0
}

object LimitDef {
  implicit def instance[Par <: HList, paramR <: HList]
  (implicit handler: LimitHandler,
   paramNames: Reify.Aux[Par, paramR],
   toList: ToList[paramR, Symbol],
   ec: EC): LimitDef[Par] = new LimitDef[Par] {
    def directive(params: HList, key: String): Directive0 = Directive { f =>
      ctx =>
        handler.check(Pattern(key, params)) flatMap {
          case LimitHandler.Exceeded(rate) =>
            val parNames = toList(paramNames()).map(_.name)
            Future.successful(Rejected(List(
              LimitRejection(key, rate.count, rate.duration.toString, parNames))))
          case LimitHandler.Success => f(())(ctx)
        }
    }
  }
}