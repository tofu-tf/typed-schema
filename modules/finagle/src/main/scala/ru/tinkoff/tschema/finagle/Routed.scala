package ru.tinkoff.tschema.finagle
import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import cats.instances.double._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.reducible._
import cats.{Apply, Monad, Order, SemigroupK}
import com.twitter.finagle.{Service, http}
import ru.tinkoff.tschema.finagle.Rejection.NotFound
import ru.tinkoff.tschema.finagle.Routed.SegmentPattern
import ru.tinkoff.tschema.param.ParamSource

import scala.util.matching.Regex

final case class Path(full: String, matchedSize: Int = 0) {
  def matched: CharSequence   = full.substring(0, matchedSize)
  def unmatched: CharSequence = full.substring(matchedSize)
  def matchMore(size: Int)    = Path(full, matchedSize + size)
}

trait Routed[F[_]] extends SemigroupK[F] {
  implicit def FMonad: Monad[F]

  def request: F[http.Request]
  def path: F[CharSequence]
  def matched: F[Int]
  def setMatched[A](m: Int): F[Unit]
  def reject[A](rejection: Rejection): F[A]
  def rejectMany[A](rejections: Rejection*): F[A]

  def addMatched[A](i: Int): F[Unit] = matched >>= (m => setMatched(m + i))

  def matchedPath: F[CharSequence]   = (path, matched).mapN(_.subSequence(0, _))
  def unmatchedPath: F[CharSequence] = (path, matched).mapN((p, m) => p.subSequence(m, p.length()))

  def customSegment(pattern: Regex, groupId: Int): F[Option[CharSequence]] =
    for {
      path     <- matchedPath
      matchOpt = pattern.findPrefixMatchOf(path)
      _        <- matchOpt.traverse_(m => addMatched(m.end - m.start))
    } yield matchOpt.map(_.group(groupId))

  def segment: F[Option[CharSequence]] = customSegment(SegmentPattern, 1)

  def checkPrefix(pref: CharSequence): F[Unit] =
    for {
      path <- matchedPath
      i    = Routed.commonPathPrefix(path, pref)
      res  <- if (i < 0) reject[Unit](NotFound) else if (i > 0) addMatched(i) else FMonad.unit
    } yield res

  def checkPath(path: CharSequence): F[Unit] =
    for {
      path <- matchedPath
      i    = Routed.commonPathPrefix(path, path)
      res  <- if (i == path.length()) addMatched(i) else reject[Unit](NotFound)
    } yield res

  def checkPathEnd: F[Unit] = checkPath("")
}

object Routed {
  def apply[F[_]](implicit instance: Routed[F]): Routed[F] = instance

  def request[F[_]](implicit routed: Routed[F]): F[http.Request]              = routed.request
  def matchedPath[F[_]: Apply](implicit routed: Routed[F]): F[CharSequence]   = routed.matchedPath
  def unmatchedPath[F[_]: Apply](implicit routed: Routed[F]): F[CharSequence] = routed.unmatchedPath
  def reject[F[_], A](rejection: Rejection)(implicit routed: Routed[F]): F[A] = routed.reject(rejection)
  def checkPathEnd[F[_], A](implicit routed: Routed[F]): F[Unit]              = routed.checkPathEnd

  val SegmentPattern = "\\/?([^\\/]*)".r

  def customSegment[F[_]](pattern: Regex, groupId: Int)(implicit routed: Routed[F]): F[Option[CharSequence]] =
    routed.customSegment(pattern, groupId)

  def segment[F[_]](implicit routed: Routed[F]): F[Option[CharSequence]] = routed.segment

  def prefix[F[_]](pref: CharSequence)(implicit routed: Routed[F]): F[Unit] = routed.checkPrefix(pref)

  private[Routed] def commonPathPrefix(path: CharSequence, pref: CharSequence): Int =
    if (pref.length() <= path.length() && path.subSequence(0, pref.length()) == pref) pref.length()
    else if (pref.length() < path.length() && path.charAt(0) == '/' && path.subSequence(1, pref.length() + 1) == pref)
      pref.length() + 1
    else -1

  object implicits {
    implicit def extractRoutedMonad[F[_]](implicit F: Routed[F]): Monad[F] = F.FMonad
  }
}

trait Runnable[F[_], G[_]] extends FunctionK[G, F] {
  def run(fresp: F[http.Response]): G[Service[http.Request, http.Response]]
}

trait ParseBody[F[_], A] {
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
