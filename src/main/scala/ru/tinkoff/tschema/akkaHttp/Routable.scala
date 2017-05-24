package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route

import scala.annotation.implicitNotFound
import scala.concurrent.Future

@implicitNotFound("could not route ${In} knowing that result should be ${Out}")
trait Routable[In, Out] {
  def route(out: In): Route
}

object Routable {
  object syntax {
    implicit class RoutableOps[In](val in: In) extends AnyVal {
      def route[Out](implicit routable: Routable[In, Out]): Route = routable.route(in)
    }
  }

  implicit def single[A](implicit marshaller: ToResponseMarshaller[A]): Routable[A, A] =
    new Routable[A, A] {
      def route(res: A): Route = complete(res)
    }

  implicit def future[A](implicit marshaller: ToResponseMarshaller[A]): Routable[Future[A], A] =
    new Routable[Future[A], A] {
      def route(res: Future[A]): Route = complete(res)
    }
}