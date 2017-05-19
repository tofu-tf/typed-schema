package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.typeDSL.{Transform, TransformReq}
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record._
import _root_.shapeless.ops.record._
import ru.tinkoff.tschema.common.HasReq

import scala.concurrent.{ExecutionContext, Future}

trait ServeMiddle[x, P <: HList, key] extends ServePartial[x, P] {
  type Input <: HList
  def apply(f: Input ⇒ Route, provide: Provide[P]): Route
}

object ServeMiddle {
  type Aux[x, P <: HList, key, I] = ServeMiddle[x, P, key] {type Input = I}

  implicit def transform1[srcType, resType, srcName <: Symbol, resName <: Symbol, key, t, P <: HList]
  (implicit ec: ExecutionContext,
   transform: TransformInput1[srcType, resType, t],
   selector: Selector.Aux[P, srcName, srcType]
  ): ServeMiddle.Aux[Transform[srcName, resName, t, srcType, resType], P, key, FieldType[resName, resType] :: P] =
    new ServeMiddle[Transform[srcName, resName, t, srcType, resType], P, key] {
      type Input = FieldType[resName, resType] :: P
      def apply(f: Input ⇒ Route, provide: Provide[P]): Route =
        provide(p ⇒ ctx ⇒ for {
          res ← transform(selector(p))
          cons = field[resName](res) :: p
          result ← f(cons)(ctx)
        } yield result)
    }

  implicit def transformReq1[srcType, resType, srcName <: Symbol, resName <: Symbol, key, req, t, P <: HList]
  (implicit ec: ExecutionContext,
   transform: TransformInputReq1.Aux[srcType, resType, t, req],
   req: req,
   selector: Selector.Aux[P, srcName, srcType]
  ): ServeMiddle.Aux[TransformReq[srcName, resName, t, srcType, resType, req], P, key, FieldType[resName, resType] :: P] =
    new ServeMiddle[TransformReq[srcName, resName, t, srcType, resType, req], P, key] {
      type Input = FieldType[resName, resType] :: P
      def apply(f: Input ⇒ Route, provide: Provide[P]): Route =
        provide(p ⇒ ctx ⇒ for {
          res ← transform(selector(p))
          cons = field[resName](res) :: p
          result ← f(cons)(ctx)
        } yield result)
    }
}

trait TransformInputReq1[a, b, x] extends HasReq {
  type Req
  def apply(x: a)(implicit ec: ExecutionContext, req: Req): Future[b]
}

trait TransformInput1[a, b, x] {
  def apply(x: a)(implicit ec: ExecutionContext): Future[b]
}

trait TransformInput1Object[a, b]{
  def transform(x: a)(implicit ec: ExecutionContext): Future[b]

  implicit val transformInstance = new TransformInput1[a, b, this.type] {
    def apply(x: a)(implicit ec: ExecutionContext): Future[b] = transform(x)
  }
}

object TransformInputReq1 {
  type Aux[a, b, t, r] = TransformInputReq1[a, b, t] {type Req = r}
}


