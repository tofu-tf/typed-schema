package ru.tinkoff.tschema.http4s

import cats.{ Applicative, Defer }
import cats.data.OptionT.none
import cats.data.{ Kleisli, OptionT }
import org.http4s.dsl.impl.Path
import org.http4s.{ HttpRoutes, Request, Response }

object Routing {

  type Routes[F[_]] = Kleisli[OptionT[F, *], Routed[F], Response[F]]

  object Routes {
    implicit class RoutesOps[F[_]](val routes: Routes[F]) extends AnyVal {
      def toHttpRoutes(implicit F: Defer[F]): HttpRoutes[F] =
        HttpRoutes { req => routes(Routed(req, Path(req.pathInfo))) }
    }
  }

  def apply[F[_]](run: Routed[F] => OptionT[F, Response[F]])(implicit F: Defer[F]): Routes[F] =
    Kleisli { req => OptionT(F.defer(run(req).value)) }

  def liftF[F[_] : Defer](fr: OptionT[F, Response[F]]): Routes[F] = apply(_ => fr)

  def request[F[_] : Defer](run: Request[F] => OptionT[F, Response[F]]): Routes[F] = apply(req => run(req.request))

  def empty[F[_] : Defer : Applicative]: Routes[F] = apply(_ => none)
}
