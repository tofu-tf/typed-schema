package tschema.akkaHttp.auth

import akka.http.scaladsl.server.directives.SecurityDirectives
import akka.http.scaladsl.server.directives.AuthenticationDirective

trait Authenticator[T] {
  def directive(realm: String): AuthenticationDirective[T]
}

trait BasicAuthenticator[T] extends Authenticator[T]

object BasicAuthenticator extends SecurityDirectives {
  def apply[T](implicit ba: BasicAuthenticator[T]): BasicAuthenticator[T] = ba

  def of[T](f: Authenticator[T]): BasicAuthenticator[T]                  = authenticateBasic(_, f)
  def partial[T](f: AuthenticatorPF[T]): BasicAuthenticator[T]           = authenticateBasicPF(_, f)
  def async[T](f: AsyncAuthenticator[T]): BasicAuthenticator[T]          = authenticateBasicAsync(_, f)
  def asyncPartial[T](f: AsyncAuthenticatorPF[T]): BasicAuthenticator[T] = authenticateBasicPFAsync(_, f)
}

trait BearerAuthenticator[T] extends Authenticator[T]

object BearerAuthenticator extends SecurityDirectives {
  def apply[T](implicit ba: BearerAuthenticator[T]): BearerAuthenticator[T] = ba

  def of[T](f: Authenticator[T]): BearerAuthenticator[T]                  = authenticateOAuth2(_, f)
  def partial[T](f: AuthenticatorPF[T]): BearerAuthenticator[T]           = authenticateOAuth2PF(_, f)
  def async[T](f: AsyncAuthenticator[T]): BearerAuthenticator[T]          = authenticateOAuth2Async(_, f)
  def asyncPartial[T](f: AsyncAuthenticatorPF[T]): BearerAuthenticator[T] = authenticateOAuth2PFAsync(_, f)
}
