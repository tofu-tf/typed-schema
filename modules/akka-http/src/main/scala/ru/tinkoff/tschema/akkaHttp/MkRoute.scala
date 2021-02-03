package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import ru.tinkoff.tschema.typeDSL.DSLDef
import shapeless.HList
import akka.http.scaladsl.server.Directives._
import ru.tinkoff.tschema.macros._

import language.experimental.macros

object MkRoute {

  def apply[Def <: DSLDef, Impl](definition: => Def)(impl: Impl): Route =
    macro MakerMacro.makeRouteHNil[Skip, macroInterface.type, Def, Impl, Route]

  def of[Def <: DSLDef, Impl, In <: HList](definition: => Def)(impl: Impl)(input: In): Route =
    macro MakerMacro.makeRoute[Skip, macroInterface.type, Def, Impl, Route, In]

  object macroInterface {
    class ResultPA1[Out] {
      def apply[In <: HList, Impl](in: In)(impl: Impl)(key: String, groups: String*): Route =
        macro MakerMacro.makeResult[Skip, In, Out, Impl, Route]
    }
    def makeResult[F[_], Out]: ResultPA1[Out]    = new ResultPA1[Out]
    def concatResults(x: Route, y: Route): Route = x ~ y

    def serve[F[_], T] = new ServePA[T]

    def route[Res](res: => Res) = new RoutePA(res)

    class RoutePA[Res](res: => Res) {
      def apply[F[_], In, Out](in: In)(implicit routable: RoutableIn[In, Res, Out]): Route = pathEnd(
        routable.route(in, res)
      )
    }

    class ServePA[T] {
      def apply[In <: HList](in: In)(implicit serve: Serve[T, In]) = new ServePA2[T, In, serve.Out](in)(serve)
    }

    class ServePA2[T, In <: HList, Out <: HList](val in: In)(srv: Serve.Aux[T, In, Out]) {
      def apply(f: Out => Route): Route = srv.directive(in).tapply { case Tuple1(out) => f(out) }
    }
  }
}
