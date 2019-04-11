package ru.tinkoff.tschema.finagle
import java.nio.CharBuffer

import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import cats.instances.double._
import cats.instances.string._
import cats.{Order, SemigroupK}
import com.twitter.finagle.{Service, http}

final case class Path(full: String, matchedSize: Int) {
  def matched: CharSequence   = CharBuffer.wrap(full, 0, matchedSize)
  def unmatched: CharSequence = CharBuffer.wrap(full, matchedSize)
}

trait Routed[F[_]] extends SemigroupK[F] {
  def request: F[http.Request]
  def path: F[String]
  def matchedPath: F[String]
  def matchNext[A](fa: F[A], length: Int): F[A]
  def reject[A](rejection: Rejection): F[A]
}

trait Runnable[F[_], G[_]] extends FunctionK[G, F] {
  def run(fresp: F[http.Response]): G[Service[http.Request, http.Response]]
}

trait Parse[F[_], A] {
  def parse: F[A]
}

trait Complete[F[_], A] {
  def complete(a: A): F[http.Response]
}

sealed abstract class Rejection(val priority: Double, val status: http.Status) {
  def message: String = ""
}

object Rejection {
  case object NotFound                                   extends Rejection(0, http.Status.NotFound)
  case class WrongMethod(method: String)                 extends Rejection(1, http.Status.MethodNotAllowed)
  case class MissingParam(name: String)                  extends Rejection(2, http.Status.BadRequest)
  case class MalformedParam(name: String, error: String) extends Rejection(3, http.Status.BadRequest)

  implicit val order: Order[Rejection] = Order.by(_.priority)

  type Handler = NonEmptyMap[String, Rejection] => http.Response

  val defaultHandler: Handler = map => http.Response(map.maximum.status)

}
