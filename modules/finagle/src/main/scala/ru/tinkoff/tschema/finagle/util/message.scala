package ru.tinkoff.tschema.finagle.util
import com.twitter.finagle.http.{Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import ru.tinkoff.tschema.finagle.{Complete, ParseBody, Rejection, Routed}
import Routed.implicits._
import cats.{Applicative, Functor}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

object message {
  def stringResponse(s: String): Response = {
    val resp = Response()
    resp.setContentType("text/plain")
    resp.setContentString(s)
    resp
  }

  def jsonResponse(s: String): Response = {
    val resp = Response()
    resp.setContentTypeJson()
    resp.setContentString(s)
    resp
  }

  def parseRequest[F[_]: Routed, A](f: String => Either[Throwable, A]): F[A] =
    Routed.request.flatMap(req =>
      f(req.contentString).fold(fail => Routed.reject(Rejection.body(fail.getMessage)), res => res.pure[F]))

  def stringComplete[F[_]: Applicative, A](f: A => String): Complete[F, A] =
    a => stringResponse(f(a)).pure[F]

  def fstringComplete[F[_]: Functor, A](f: A => String): Complete[F, F[A]] =
    fa => fa.map(a => stringResponse(f(a)))

  def jsonComplete[F[_]: Applicative, A](f: A => String): Complete[F, A] =
    a => jsonResponse(f(a)).pure[F]

  def fjsonComplete[F[_]: Functor, A](f: A => String): Complete[F, F[A]] =
    fa => fa.map(a => jsonResponse(f(a)))

  def jsonBodyParse[F[_]: Routed, A](f: String => Either[Throwable, A]): ParseBody[F, A] =
    () => parseRequest(f)
}
