package ru.tinkoff.tschema.finagle
import cats.{Foldable, MonoidK, Order, SemigroupK}
import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import com.twitter.finagle.{Service, http}
import cats.syntax.reducible._
import cats.instances.double._
import cats.instances.string._

trait Routed[F[_]] extends SemigroupK[F] {
  def request: F[http.Request]
  def path: F[String]
  def withPath[A](fa: F[A], f: String => String): F[A]
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

sealed abstract class Rejection(val priority: Double, val status: http.Status, val message: String = "")

object Rejection {
  case object NotFound                                   extends Rejection(0, http.Status.NotFound)
  case class WrongMethod(method: String)                 extends Rejection(1, http.Status.MethodNotAllowed)
  case class MissingParam(name: String)                  extends Rejection(2, http.Status.BadRequest)
  case class MalformedParam(name: String, error: String) extends Rejection(3, http.Status.BadRequest)

  implicit val order: Order[Rejection] = Order.by(_.priority)

  type Handler = NonEmptyMap[String, Rejection] => http.Response

  val defaultHandler: Handler = map => {
    Foldable[NonEmptyMap[String, ?]]
    val best = map.maximum
    http.Response(best.status)
  }

}
