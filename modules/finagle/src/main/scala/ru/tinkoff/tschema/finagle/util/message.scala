package ru.tinkoff.tschema.finagle.util
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Functor, Monad}
import com.twitter.finagle.http.{MediaType, Response, Status}
import ru.tinkoff.tschema.finagle.{Complete, LiftHttp, ParseBody, Rejection, Routed}

object message {

  def response(s: String, contentType: String, status: Status = Status.Ok): Response = {
    val resp = Response(status)
    resp.setContentType(contentType)
    resp.setContentString(s)
    resp
  }

  def stringResponse(s: String, status: Status = Status.Ok): Response = response(s, "text/plain", status)
  def jsonResponse(s: String, status: Status = Status.Ok): Response   = response(s, MediaType.Json, status)

  def emptyComplete[F[_]: Applicative, A, B](status: Status = Status.Ok): Complete[F, A, B] = _ => Response(status).pure[F]

  def parseRequest[F[_]: Routed: Monad, A](f: String => Either[Throwable, A]): F[A] =
    Routed.request.flatMap(req =>
      f(req.contentString).fold(fail => Routed.reject(Rejection.body(fail.getMessage)), res => res.pure[F]))

  def stringComplete[F[_]: Applicative, A](f: A => String, status: Status = Status.Ok): Complete[F, A, A] =
    a => stringResponse(f(a)).pure[F]

  def fstringComplete[F[_], G[_]: Functor, A](f: A => String, status: Status = Status.Ok)(implicit lift: LiftHttp[F, G]): Complete[F, A, G[A]] =
    fa => lift(fa.map(a => stringResponse(f(a))))

  def jsonComplete[F[_]: Applicative, A](f: A => String, status: Status = Status.Ok): Complete[F, A, A] =
    a => jsonResponse(f(a)).pure[F]

  def fjsonComplete[F[_], G[_]: Functor, A](f: A => String, status: Status = Status.Ok)(implicit lift: LiftHttp[F, G]): Complete[F, A, G[A]] =
    fa => lift(fa.map(a => jsonResponse(f(a))))

  def jsonBodyParse[F[_]: Routed: Monad, A](f: String => Either[Throwable, A]): ParseBody[F, A] =
    () => parseRequest(f)
}
