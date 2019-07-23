package ru.tinkoff.tschema.finagle
import cats.Monad
import ru.tinkoff.tschema.typeDSL._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.order._
import cats.instances.string._
import shapeless.HList

private[finagle] trait MethodDirectives { self: Serve.type =>
  implicit def serveCheckMethod[T <: DSLMethod, F[_]: Routed: Monad, In](implicit method: CheckMethod[T]): Filter[T, F, In] =
    check(Routed.request.flatMap[Unit] { req =>
      if (req.method.name === method.name) ().pure[F]
      else
        Routed.reject[F, Unit](
          Rejection.wrongMethod(method.name)
        )
    })
}

final case class CheckMethod[T](name: String)

object CheckMethod {
  implicit val checkGet: CheckMethod[Get] = CheckMethod("get")

}
