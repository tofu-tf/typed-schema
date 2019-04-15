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
  def apply[F[_], Def <: DSLDef, Impl](definition: => Def)(impl: Impl): F[Response] =
    macro MakerMacro.makeRouteHNil[macroInterface.type, Def, Impl, F[Response]]

  def of[F[_], Def <: DSLDef, Impl, In <: HList](definition: => Def)(impl: Impl)(input: In): F[Response] =
    macro MakerMacro.makeRoute[macroInterface.type, Def, Impl, F[Response], In]

  object macroInterface {
    import Routed.implicits._

    def makeResult[Out]: ResultPA1[Out] = new ResultPA1[Out]

    def concatResults[F[_]: Routed](x: F[Response], y: F[Response]): F[Response] =
      x combineK y

    def serve[T] = new ServePA[T]

    def route[Res](res: => Res) = new RoutePA(res)

    class ResultPA1[Out] {
      def apply[F[_], In <: HList, Impl](in: In)(impl: Impl)(key: String): F[Response] =
        macro MakerMacro.makeResult[In, Out, Impl, F[Response]]
    }

    class RoutePA[Res](res: => Res) {
      def apply[F[_]: Routed, In, Out](in: In)(implicit complete: Complete[F, Res]): F[Response] =
        Routed.checkPathEnd *> complete.complete(res)
    }

    class ServePA[T] {
      def apply[F[_]: FlatMap, In, Out](in: In)(implicit serve: Serve[T, F, In, Out]) = new ServePA2[F, T, In, Out](in)(serve)
    }

    class ServePA2[F[_]: FlatMap, T, In, Out](val in: In)(srv: Serve[T, F, In, Out]) {
      def apply(f: Out => F[Response]): F[Response] = srv.process(in).flatMap(f)
    }
  }
}
