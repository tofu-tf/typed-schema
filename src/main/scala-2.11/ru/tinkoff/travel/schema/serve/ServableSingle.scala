package ru.tinkoff.travel.schema.serve

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server._
import Directives._
import shapeless.ops.function._
import shapeless.{HList, HNil}

import scala.concurrent.Future

trait ServableSingle[Input <: HList, Output] {
  def route(x: Input): Route
}

object ServableSingle {
  implicit def fromFunctionServable[F, O, I <: HList]
  (f: F)
  (implicit toProduct: FnToProduct.Aux[F, I ⇒ O], marshallable: ToResponseMarshaller[O]) = new ServableSingle[I, O]{
    def route(in: I): Route = complete(toProduct(f)(in))
  }
}

trait ToServableSingle[x, I <: HList, O] extends (x ⇒ ServableSingle[I, O]) {
  def apply(v1: x): ServableSingle[I, O]
}

object ToServableSingle {
  implicit def fromValueServable[O]
  (implicit marshallable: ToResponseMarshaller[O]) = new ToServableSingle[O, HNil, O] {
    def apply(x: O) = new ServableSingle[HNil, O]{
      def route(_y: HNil): Route = complete(x)
    }
  }

  implicit def fromFutureServable[O]
  (implicit marshallable: ToResponseMarshaller[Future[O]]) = new ToServableSingle[Future[O], HNil, O]{
    def apply(x: Future[O]) = new ServableSingle[HNil, O]{
      def route(_y: HNil): Route = complete(x)
    }
  }

  implicit def fromFunctionServable[F, O, I <: HList]
  (implicit toProduct: FnToProduct.Aux[F, I ⇒ O], marshallable: ToResponseMarshaller[O]) = new ToServableSingle[F, I, O]{
    def apply(f: F)  = new ServableSingle[I, O]{
      def route(x: I): Route = complete(toProduct(f)(x))
    }
  }

  implicit def fromFunctionFutureServable[F, O, I <: HList]
  (implicit toProduct: FnToProduct.Aux[F, I ⇒ Future[O]], marshallable: ToResponseMarshaller[Future[O]]) = new ToServableSingle[F, I, O]{
    def apply(f: F) = new ServableSingle[I, O]{
      def route(x: I): Route = complete(toProduct(f)(x))
    }
  }
}