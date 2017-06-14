package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.server.{Directive1, Route}
import ru.tinkoff.tschema.typeDSL.DSLDef
import shapeless.HList
import akka.http.scaladsl.server.Directives._

import language.experimental.macros

object MkRoute {
  def apply[Def <: DSLDef, Impl](definition: => Def)(impl: Impl): Route = macro MakerMacro.makeRoute[macroInterface.type, Def, Impl]

  object macroInterface {
    class ResultApplier[Out] {
      def apply[In <: HList, Impl](in: In)(impl: Impl)(key: String): Route = macro MakerMacro.makeResult[In, Out, Impl]
    }

    def makeResult[T](implicit result: Result[T]): ResultApplier[result.Out] = new ResultApplier[result.Out]
    def concatResults(x: Route, y: Route): Route = x ~ y

    implicit class RoutableOps[In](val in: In) extends AnyVal {
      def route[Out](implicit routable: Routable[In, Out]): Route = routable.route(in)
    }

    implicit class ServeOps[In <: HList](val in: In) extends AnyVal {
      def serve[T](implicit serve: Serve[T, In]): Directive1[serve.Out] = serve.directive(in)
    }
  }
}
