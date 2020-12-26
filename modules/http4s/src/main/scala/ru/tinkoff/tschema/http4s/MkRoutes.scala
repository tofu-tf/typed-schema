package ru.tinkoff.tschema.http4s

import cats.data.OptionT
import cats.{ Applicative, Defer, Monad }
import cats.data.OptionT.none
import cats.syntax.semigroupk._
import org.http4s.dsl.Http4sDsl
import org.http4s.{ EntityEncoder, HttpRoutes }
import ru.tinkoff.tschema.http4s.Routing.Routes
import ru.tinkoff.tschema.macros.MakerMacro
import ru.tinkoff.tschema.typeDSL.DSLDef
import shapeless.HList

object MkRoutes {
  def apply[F[_]] = new MkApply[F]

  class MkApply[F[_]] {
    def apply[Def <: DSLDef, Impl](definition: => Def)(impl: Impl): HttpRoutes[F] =
      macro RoutesMacro.convert[F, Def, Impl, HttpRoutes[F]]

    def of[Def <: DSLDef, Impl, In <: HList](definition: => Def)(impl: Impl)(input: In): HttpRoutes[F] =
      macro RoutesMacro.convertIn[F, Def, Impl, In, HttpRoutes[F]]

    def makeRoutes[Def <: DSLDef, Impl](definition: => Def)(impl: Impl): Routes[F] =
      macro MakerMacro.makeRouteHNil[F, macroInterface.type, Def, Impl, Routes[F]]

    def makeRoutes[Def <: DSLDef, Impl, In <: HList](definition: => Def)(impl: Impl)(input: In): Routes[F] =
      macro MakerMacro.makeRoute[F, macroInterface.type, Def, Impl, Routes[F], In]
  }

  object macroInterface {
    def makeResult[F[_], Out]: ResultPA1[F, Out] = new ResultPA1[F, Out]
    def serve[F[_], T] = new ServePA[F, T]
    def route[Res](res: => Res) = new RoutePA[Res](res)
    def concatResults[F[_] : Monad](x: Routes[F], y: Routes[F]): Routes[F] = x <+> y

    class ResultPA1[F[_], Out] {
      def apply[In <: HList, Impl](in: In)(impl: Impl)(key: String, groups: String*): Routes[F] =
        macro MakerMacro.makeResult[F, In, Out, Impl, Routes[F]]
    }

    class RoutePA[Res](res: => Res) {
      def apply[F[_] : Defer : Applicative, In, Out](in: In)(implicit w: EntityEncoder[F, Res]): Routes[F] = {
        object io extends Http4sDsl[F]
        import io._
        Routing { _.matchEnd().map(_ => OptionT.liftF(Ok(res))).getOrElse(none) }
      }
    }

    class ServePA[F[_], T] {
      def apply[In, Out](in: In)(implicit serve: Serve[T, F, In, Out]) =
        new ServePA2[F, T, In, Out](in)(serve)
    }

    class ServePA2[F[_], T, In, Out](val in: In)(srv: Serve[T, F, In, Out]) {
      def apply(f: Out => Routes[F]): Routes[F] = srv.process(f, in)
    }
  }
}
