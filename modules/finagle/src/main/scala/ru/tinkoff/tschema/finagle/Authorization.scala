package ru.tinkoff.tschema.finagle
import cats.Applicative
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import ru.tinkoff.tschema.finagle.Authorization.{Basic, Bearer, Kind}
import ru.tinkoff.tschema.typeDSL.{ApiKeyAuth, BasicAuth, BearerAuth, CanHoldApiKey}
import shapeless.HList
import Routed.implicits._
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.finagle.Rejection.unauthorized

trait Authorization[K <: Kind, F[_], A] {
  def apply(s: Option[String]): F[A]
}

object Authorization {
  sealed trait Kind

  case object Basic  extends Kind
  case object Bearer extends Kind

  type Basic  = Basic.type
  type Bearer = Bearer.type

  def require[K <: Kind] = new Require[K](true)
  class Require[K <: Kind](val dummy: Boolean) extends AnyVal {
    def apply[F[_]: Routed, A](f: String => F[A]): Authorization[K, F, A] = {
      case Some(s) => f(s)
      case None    => Routed.reject(unauthorized)
    }
  }

  def basic[F[_], A](implicit auth: Authorization[Basic, F, A]): Authorization[Basic, F, A]    = auth
  def bearer[F[_], A](implicit auth: Authorization[Bearer, F, A]): Authorization[Bearer, F, A] = auth

  implicit def optional[K <: Kind, F[_]: Applicative, A](
      implicit auth: Authorization[K, F, A]): Authorization[K, F, Option[A]] =
    _.traverse(s => auth(Some(s)))

  implicit def applicative[K <: Kind, F[_]: Applicative]: Applicative[Authorization[K, F, ?]] =
    new Applicative[Authorization[K, F, ?]] {
      def pure[A](x: A): Authorization[K, F, A] = _ => x.pure[F]
      def ap[A, B](ff: Authorization[K, F, A => B])(fa: Authorization[K, F, A]): Authorization[K, F, B] =
        os => ff(os) ap fa(os)
    }
}

private[finagle] trait ServeAuthInstances { self: Serve.type =>
  implicit def apiKeyAuthServe[F[_]: Routed, realm, Param <: CanHoldApiKey, In, Out](
      implicit serve: Serve[Param, F, In, Out]): Serve[ApiKeyAuth[realm, Param], F, In, Out] =
    serve.as[ApiKeyAuth[realm, Param]]

  private def authServe[F[_]: Routed, realm, name <: Symbol: Name, x, In <: HList, K <: Kind, atom](
      implicit auth: Authorization[K, F, x]): Add[atom, F, In, name, x] =
    serveAdd(Routed.request.flatMap(r => auth(r.authorization)))

  implicit def basicAuthServe[F[_]: Routed, realm, name <: Symbol: Name, x: Authorization[Basic, F, ?], In <: HList]
    : Add[BasicAuth[realm, name, x], F, In, name, x] = authServe[F, realm, name, x, In, Basic, BasicAuth[realm, name, x]]

  implicit def bearerAuthServe[F[_]: Routed, realm, name <: Symbol: Name, x: Authorization[Bearer, F, ?], In <: HList]
    : Add[BearerAuth[realm, name, x], F, In, name, x] = authServe[F, realm, name, x, In, Bearer, BearerAuth[realm, name, x]]
}
