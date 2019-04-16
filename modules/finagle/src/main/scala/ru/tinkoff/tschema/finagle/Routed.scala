package ru.tinkoff.tschema.finagle
import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import cats.instances.double._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.order._
import cats.syntax.functor._
import cats.syntax.reducible._
import cats.{Apply, Foldable, Monad, Monoid, Order, SemigroupK}
import com.twitter.finagle.http.Status._
import com.twitter.finagle.http.Version
import com.twitter.finagle.{Service, http}
import com.twitter.io.Buf
import ru.tinkoff.tschema.finagle.Rejection.{MalformedParam, MissingParam, notFound}
import ru.tinkoff.tschema.finagle.Routed.SegmentPattern
import ru.tinkoff.tschema.param.ParamSource
import com.twitter.io

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

  def rejectMany[A](rejections: Rejection*): F[A] = reject(Foldable[List].fold(rejections.toList))

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
      res  <- if (i < 0) reject[Unit](notFound) else if (i > 0) addMatched(i) else FMonad.unit
    } yield res

  def checkPath(path: CharSequence): F[Unit] =
    for {
      path <- matchedPath
      i    = Routed.commonPathPrefix(path, path)
      res  <- if (i == path.length()) addMatched(i) else reject[Unit](notFound)
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
  def parse(): F[A]
}

trait Complete[F[_], A] {
  def complete(a: A): F[http.Response]
}

final case class Rejection(
    priority: Double,
    status: http.Status = NotFound,
    path: String = "",
    wrongMethod: List[String] = Nil,
    missing: List[MissingParam] = Nil,
    malformed: List[MalformedParam] = Nil,
    unauthorized: Boolean = false,
    messages: List[String] = Nil
) {
  def withPath(p: String): Rejection   = copy(path = p)
  def addMessage(s: String): Rejection = copy(messages = s :: messages)
}

object Rejection {
  val notFound                                        = Rejection(0)
  def wrongMethod(method: String)                     = Rejection(1, MethodNotAllowed, wrongMethod = List(method))
  def missingParam(name: String, source: ParamSource) = Rejection(3, BadRequest, missing = List(MissingParam(name, source)))
  def malformedParam(name: String, error: String, source: ParamSource) =
    Rejection(4, BadRequest, malformed = List(MalformedParam(name, error, source)))
  def body(error: String) = Rejection(5, BadRequest, messages = List(error))
  val unauthorized        = Rejection(6, Unauthorized)
  case class MissingParam(name: String, source: ParamSource)
  case class MalformedParam(name: String, error: String, source: ParamSource)

  implicit val rejectionMonoid: Monoid[Rejection] = new Monoid[Rejection] {
    def empty: Rejection = notFound
    def combine(x: Rejection, y: Rejection): Rejection =
      if (x < y) combine(y, x)
      else
        Rejection(
          x.priority,
          x.status,
          x.path,
          x.wrongMethod ::: y.wrongMethod,
          x.missing ::: y.missing,
          x.malformed ::: y.malformed,
          x.unauthorized || y.unauthorized,
          x.messages ::: y.messages
        )
  }

  implicit val order: Order[Rejection] =
    (x, y) =>
      x.priority compare y.priority match {
        case 0 => x.path.length compare y.path.length
        case i => i
    }

  type Handler = Rejection => http.Response

  val defaultHandler: Handler = rej =>
    http.Response(
      Version.Http10,
      rej.status,
      io.Reader.fromBuf(rej.messages.headOption.fold(Buf.Empty)(Buf.Utf8(_)))
  )

}
