package ru.tinkoff.tschema.finagle
import java.nio.CharBuffer

import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import cats.instances.double._
import cats.instances.string._
import cats.{Apply, Order, SemigroupK}
import com.twitter.finagle.{Service, http}
import cats.syntax.reducible._
import cats.syntax.apply._
import ru.tinkoff.tschema.finagle.Rejection.MissingParam
import ru.tinkoff.tschema.param.ParamSource

final case class Path(full: String, matchedSize: Int = 0) {
  def matched: CharSequence   = full.substring(0, matchedSize)
  def unmatched: CharSequence = full.substring(matchedSize)
  def matchMore(size: Int)    = Path(full, matchedSize + size)
}

trait Routed[F[_]] extends SemigroupK[F] {
  def request: F[http.Request]
  def matched: F[Int]
  def setMatched[A](m: Int): F[Unit]
  def reject[A](rejection: Rejection): F[A]
  def rejectMany[A](rejections: Rejection*): F[A]
}

object Routed {
  def request[F[_]](implicit routed: Routed[F]): F[http.Request] = routed.request
  def matchedPath[F[_]: Apply](implicit routed: Routed[F]): F[String] =
    (routed.request, routed.matched).mapN(_.path.substring(0, _))
  def unmatchedPath[F[_]: Apply](implicit routed: Routed[F]): F[String] =
    (routed.request, routed.matched).mapN(_.path.substring(_))
  def reject[F[_], A](rejection: Rejection)(implicit routed: Routed[F]): F[A] = routed.reject(rejection)
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
  case object NotFound                                                        extends Rejection(0, http.Status.NotFound)
  case class WrongMethod(method: String)                                      extends Rejection(1, http.Status.MethodNotAllowed)
  case class MissingParam(name: String, source: ParamSource)                  extends Rejection(2, http.Status.BadRequest)
  case class MalformedParam(name: String, error: String, source: ParamSource) extends Rejection(3, http.Status.BadRequest)

  implicit val order: Order[Rejection] = Order.by(_.priority)

  type Handler = NonEmptyMap[String, Rejection] => http.Response

  val defaultHandler: Handler = map => http.Response(map.maximum.status)

}
