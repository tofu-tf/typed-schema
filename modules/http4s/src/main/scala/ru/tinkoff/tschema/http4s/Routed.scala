package ru.tinkoff.tschema.http4s

import cats.effect.Sync
import cats.syntax.functor._
import cats.syntax.option._
import org.http4s.dsl.impl.Path
import org.http4s.{ EntityDecoder, Request, RequestCookie, Response, UrlForm }
import tofu.MonadThrow

case class Routed[F[_]](request: Request[F], unmatched: Path) {
  def matchPrefix(prefix: String): Option[Routed[F]] = unmatched.toList match {
    case head :: tail if head == prefix => Routed(request, Path(tail)).some
    case Nil if prefix == ""            => Routed(request, unmatched).some
    case _                              => none
  }

  def matchEnd(): Option[Routed[F]] = unmatched.toList match {
    case Nil => this.some
    case _   => none
  }

  def segment(): (Option[String], Routed[F]) = unmatched.toList match {
    case head :: tail => (head.some, Routed(request, Path(tail)))
    case _ => (none, this)
  }

  def decodeFormParams(f: Map[String, List[String]] => F[Response[F]])(implicit me: Sync[F]): F[Response[F]] =
    request.decode[UrlForm] { data => f(data.values.view.mapValues(_.toList).toMap) }

  def queryParams: Map[String, String] = request.params
  def queryMultiParams: Map[String, List[String]] = request.multiParams.view.mapValues(_.toList).toMap
  def cookies: Map[String, String] = request.cookies.map { case RequestCookie(name, content) => (name, content) }.toMap
  def headerMap: Map[String, String] = request.headers.toList.map(header => (header.name.value, header.value)).toMap
  def body[A : EntityDecoder[F, *]](implicit me: MonadThrow[F]): F[A] = request.as[A]
  def formParams(implicit me: Sync[F]): F[Map[String, List[String]]] =
    request.as[UrlForm].map(_.values.view.mapValues(_.toList).toMap)
}
