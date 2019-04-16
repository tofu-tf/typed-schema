package ru.tinkoff.tschema.finagle.util
import com.twitter.finagle.http.{Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import ru.tinkoff.tschema.finagle.{Complete, ParseBody, Rejection, Routed}
import Routed.implicits._
import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.flatMap._

object message {
  def jsonResponse(s: String): Response = {
    val resp = Response(Version.Http10, Status.Ok, Reader.fromBuf(Buf.Utf8(s)))
    resp.setContentTypeJson()
    resp
  }

  def parseRequest[F[_]: Routed, A](f: String => Either[Throwable, A]): F[A] =
    Routed.request.flatMap(req =>
      f(req.contentString).fold(fail => Routed.reject(Rejection.body(fail.getMessage)), res => res.pure[F]))

  def jsonComplete[F[_]: Applicative, A](f: A => String): Complete[F, A] =
    a => jsonResponse(f(a)).pure[F]

  def jsonBodyParse[F[_]: Routed, A](f: String => Either[Throwable, A]): ParseBody[F, A] =
    () => parseRequest(f)
}
