package tschema.finagle
import cats.{Applicative, Monad}
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import com.twitter.util.Base64StringEncoder
import tschema.finagle.Authorization.{Basic, Bearer, Kind}
import tschema.typeDSL.{ApiKeyAuth, BasicAuth, BearerAuth, CanHoldApiKey}
import shapeless.HList
import tschema.common.Name
import tschema.finagle.Rejection.unauthorized

import scala.annotation.tailrec

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

  implicit def applicative[K <: Kind, F[_]: Applicative]: Applicative[Authorization[K, F, *]] =
    new Applicative[Authorization[K, F, *]] {
      def pure[A](x: A): Authorization[K, F, A] = _ => x.pure[F]
      def ap[A, B](ff: Authorization[K, F, A => B])(fa: Authorization[K, F, A]): Authorization[K, F, B] =
        os => ff(os) ap fa(os)
    }
}

private[finagle] trait ServeAuthInstances { self: Serve.type =>
  implicit def apiKeyAuthServe[F[_]: Routed, realm, Param <: CanHoldApiKey, In, Out](
      implicit serve: Serve[Param, F, In, Out]): Serve[ApiKeyAuth[realm, Param], F, In, Out] =
    serve.as[ApiKeyAuth[realm, Param]]

  private def authServe[F[_]: Routed: Monad, realm, name <: Symbol: Name, x, In <: HList, K <: Kind, atom](
      implicit auth: Authorization[K, F, x]): Add[atom, F, In, name, x] =
    add(Routed.request.flatMap(r => auth(r.authorization)))

  implicit def basicAuthServe[F[_]: Routed: Monad, realm, name <: Symbol: Name, x: Authorization[Basic, F, *], In <: HList]
    : Add[BasicAuth[realm, name, x], F, In, name, x] = authServe[F, realm, name, x, In, Basic, BasicAuth[realm, name, x]]

  implicit def bearerAuthServe[F[_]: Routed: Monad, realm, name <: Symbol: Name, x: Authorization[Bearer, F, *], In <: HList]
    : Add[BearerAuth[realm, name, x], F, In, name, x] = authServe[F, realm, name, x, In, Bearer, BearerAuth[realm, name, x]]
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
      case _ => None
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
  def apply[K <: Kind, F[_]: Routed: Applicative, A](f: PartialFunction[String, A]): Authorization[K, F, A] = {
    case Some(s) if f.isDefinedAt(s) => f(s).pure[F]
    case _                           => Routed.reject(Rejection.unauthorized)
  }
}
