package ru.tinkoff.tschema.finagle
import cats.{FlatMap, Monad}
import com.twitter.finagle.http
import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.macros.MakerMacro
import shapeless.HList
import cats.syntax.semigroupk._
import cats.syntax.flatMap._
import cats.syntax.apply._
import ru.tinkoff.tschema.typeDSL.DSLDef

object MkService {
  def apply[F[_]] = new MkApply[F]

  class MkApply[F[_]] {
    def apply[Def <: DSLDef, Impl](definition: => Def)(impl: Impl): F[Response] =
      macro MakerMacro.makeRouteHNil[F, macroInterface.type, Def, Impl, F[Response]]

    def of[Def <: DSLDef, Impl, In <: HList](definition: => Def)(impl: Impl)(input: In): F[Response] =
      macro MakerMacro.makeRoute[F, macroInterface.type, Def, Impl, F[Response], In]
  }

  object macroInterface {
    import Routed.implicits._

    def makeResult[F[_], Out]: ResultPA1[F, Out] = new ResultPA1[F, Out]

    def concatResults[F[_]: Routed](x: F[Response], y: F[Response]): F[Response] =
      x combineK y

    def serve[F[_], T] = new ServePA[F, T]

    def route[Res](res: => Res) = new RoutePA[Res](res)

    class ResultPA1[F[_], Out] {
      def apply[In <: HList, Impl](in: In)(impl: Impl)(key: String): F[Response] =
        macro MakerMacro.makeResult[F, In, Out, Impl, F[Response]]
    }

    class RoutePA[Res](res: => Res) {
      def apply[F[_]: Routed, In, Out](in: In)(implicit complete: Complete[F, Res]): F[Response] =
        Routed.checkPathEnd(complete.complete(res))
    }

    class ServePA[F[_], T] {
      def apply[In, Out](in: In)(implicit serve: Serve[T, F, In, Out], F: FlatMap[F]) = new ServePA2[F, T, In, Out](in)(serve)
    }

    class ServePA2[F[_]: FlatMap, T, In, Out](val in: In)(srv: Serve[T, F, In, Out]) {
      def apply(f: Out => F[Response]): F[Response] = srv.process(in, f)
    }
  }
}
