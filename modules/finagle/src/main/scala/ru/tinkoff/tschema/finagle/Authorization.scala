package ru.tinkoff.tschema.finagle
import cats.{Applicative, Monad}
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import com.twitter.util.Base64StringEncoder
import ru.tinkoff.tschema.finagle.Authorization.{AuthorizationS, Basic, Bearer, Kind, OAuth2}
import ru.tinkoff.tschema.typeDSL.{ApiKeyAuth, BasicAuth, BearerAuth, CanHoldApiKey, OAuth2Auth}
import shapeless.HList
import ru.tinkoff.tschema.finagle.Rejection.unauthorized
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.utils.Provision

import scala.annotation.tailrec

trait Authorization[K <: Kind, F[_], A, R] {
  def apply(s: Option[R]): F[A]
}

object Authorization {
  sealed trait Kind

  case object Basic  extends Kind
  case object Bearer extends Kind
  case object OAuth2 extends Kind

  type Basic  = Basic.type
  type Bearer = Bearer.type
  type OAuth2 = OAuth2.type

  type AuthorizationS[K <: Kind, F[_], A] = Authorization[K, F, A, String]

  def require[K <: Kind] = new Require[K](true)
  class Require[K <: Kind](val dummy: Boolean) extends AnyVal {
    def apply[F[_]: Routed, A](f: String => F[A]): AuthorizationS[K, F, A] = {
      case Some(s) => f(s)
      case None    => Routed.reject(unauthorized)
    }
  }

  def basic[F[_], A](implicit auth: AuthorizationS[Basic, F, A]): AuthorizationS[Basic, F, A]    = auth
  def bearer[F[_], A](implicit auth: AuthorizationS[Bearer, F, A]): AuthorizationS[Bearer, F, A] = auth

  implicit def optional[K <: Kind, F[_]: Applicative, A, R](implicit
      auth: Authorization[K, F, A, R]
  ): Authorization[K, F, Option[A], R] =
    _.traverse(s => auth(Some(s)))

  implicit def applicative[K <: Kind, F[_]: Applicative, R]: Applicative[Authorization[K, F, *, R]] =
    new Applicative[Authorization[K, F, *, R]] {
      def pure[A](x: A): Authorization[K, F, A, R]                                                               = _ => x.pure[F]
      def ap[A, B](ff: Authorization[K, F, A => B, R])(fa: Authorization[K, F, A, R]): Authorization[K, F, B, R] =
        os => ff(os) ap fa(os)
    }
}

private[finagle] trait ServeAuthInstances { self: Serve.type =>
  implicit def apiKeyAuthServe[F[_]: Routed, realm, Param <: CanHoldApiKey, In, Out](implicit
      serve: Serve[Param, F, In, Out]
  ): Serve[ApiKeyAuth[realm, Param], F, In, Out] =
    serve.as[ApiKeyAuth[realm, Param]]

  private def authServe[F[_]: Routed: Monad, realm, name: Name, x, In <: HList, K <: Kind, atom](implicit
      auth: AuthorizationS[K, F, x]
  ): Add[atom, F, In, name, x] =
    add(Routed.request.flatMap(r => auth(r.authorization)))

  implicit def basicAuthServe[F[_]: Routed: Monad, realm, name: Name, x: AuthorizationS[Basic, F, *], In <: HList]
      : Add[BasicAuth[realm, name, x], F, In, name, x] =
    authServe[F, realm, name, x, In, Basic, BasicAuth[realm, name, x]]

  implicit def bearerAuthServe[F[_]: Routed: Monad, realm, name: Name, x: AuthorizationS[Bearer, F, *], In <: HList]
      : Add[BearerAuth[realm, name, x], F, In, name, x] =
    authServe[F, realm, name, x, In, Bearer, BearerAuth[realm, name, x]]

  implicit def oauth2Serve[F[_]: Routed: Monad, conf, T, R, In <: HList, name](implicit
      A: Authorization[OAuth2, F, T, R],
      P: Provision[F, R]
  ): Add[OAuth2Auth[T, R, conf, name], F, In, name, T] = add(P.provide >>= A.apply)
}

object Credentials {
  private[this] val Token        = "Basic (.*)".r
  private[this] val UsrAndPasswd = "(.*):(.*)".r

  def apply(username: String, password: String): String =
    s"Basic ${Base64StringEncoder.encode(s"$username:$password".getBytes("UTF-8"))}"

  def unapply(s: String): Option[(String, String)] =
    s match {
      case Token(t) =>
        new String(Base64StringEncoder.decode(t), "UTF-8") match {
          case UsrAndPasswd(u, p) => Some(u -> p)
          case _                  => None
        }
      case _        => None
    }

  def secure_equals(that: CharSequence, other: CharSequence): Boolean = {
    @tailrec def xor(ix: Int = 0, result: Int = 0): Int =
      if (ix < that.length) xor(ix + 1, result | (that.charAt(ix) ^ other.charAt(ix))) else result

    other.length == that.length && xor() == 0
  }
}

object BearerToken {
  private[this] val Token = "Bearer (.*)".r

  def apply(token: String): String = s"Bearer $token"

  def unapply(s: String): Option[String] = s match {
    case Token(t) => Some(t)
    case _        => None
  }
}

object SimpleAuth {
  def apply[K <: Kind, F[_]: Routed: Applicative, A, R](f: PartialFunction[R, A]): Authorization[K, F, A, R] = {
    case Some(s) if f.isDefinedAt(s) => f(s).pure[F]
    case _                           => Routed.reject(Rejection.unauthorized)
  }
}
