package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route

import scala.annotation.implicitNotFound
import scala.concurrent.Future

@implicitNotFound("could not route ${Res} knowing that result should be ${Out}")
trait Routable[Res, Out] {
  def route(res: => Res): Route
}

object Routable {

  implicit def single[A](implicit marshaller: ToResponseMarshaller[A]): Routable[A, A]         = complete(_)
  implicit def future[A](implicit marshaller: ToResponseMarshaller[A]): Routable[Future[A], A] = complete(_)
}

@implicitNotFound("could not route ${Res} knowing that result should be ${Out}")
trait RoutableIn[In, Res, Out] {
  def route(in: In, res: => Res): Route
}

object RoutableIn {
  implicit def byRoutable[Res, Out, In](implicit routable: Routable[Res, Out]): RoutableIn[In, Res, Out] =
    (_, res) => routable.route(res)
}
