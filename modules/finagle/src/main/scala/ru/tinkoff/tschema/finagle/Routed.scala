package ru.tinkoff.tschema.finagle
import cats.arrow.FunctionK
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.order._
import cats.{Apply, Contravariant, Foldable, Monad, Monoid, MonoidK, Order, SemigroupK}
import com.twitter.finagle.http.Status._
import com.twitter.finagle.http.{HttpMuxer, Request, Response, Route}
import com.twitter.finagle.{Service, http}
import ru.tinkoff.tschema.finagle.Rejection.{MalformedParam, MissingParam, notFound}
import ru.tinkoff.tschema.param.{Param, ParamSource}

import scala.util.matching.Regex

final case class Path(full: String, matchedSize: Int = 0) {
  def matched: CharSequence = full.substring(0, matchedSize)
  def unmatched: CharSequence = full.substring(matchedSize)
  def matchMore(size: Int) = Path(full, matchedSize + size)
}

trait Routed[F[_]] {
  def request: F[Request]
  def path: F[CharSequence]
  def matched: F[Int]
  def withMatched[A](m:    Int, fa: F[A]): F[A]
  def reject[A](rejection: Rejection): F[A]
}

trait RoutedPlus[F[_]] extends Routed[F] with MonoidK[F] {
  def empty[A]: F[A] = reject(Rejection.notFound)
}

object Routed extends RoutedFunctions {
  def apply[F[_]](implicit instance: Routed[F]): Routed[F] = instance

}

trait Runnable[F[_], G[_]] {
  def lift[A](fa: G[A]): F[A]
  def run(fresp:  F[Response]): G[Service[Request, Response]]

  def funK: FunctionK[G, F] = new FunctionK[G, F] {
    def apply[A](fa: G[A]): F[A] = lift(fa)
  }
}

object Runnable {
  def run[G[_]] = new Run[G]

  class Run[G[_]] {
    def apply[F[_]](resp:                F[Response])(implicit runnable: Runnable[F, G]): G[Service[Request, Response]] = runnable.run(resp)
    def all[T[_]: Foldable, F[_]](resps: T[(String, F[Response])])(
        implicit runnable:               Runnable[F, G],
        G:                               Monad[G]
    ): G[Service[Request, Response]] =
      resps
        .foldM[G, HttpMuxer](HttpMuxer) {
          case (mux, (name, svc)) => runnable.run(svc).map(s => mux withHandler Route(name, s))
        }
        .widen
  }

}

trait ParseBody[F[_], A] {
  def parse(): F[A]
}

trait CompleteIn[F[_], -In, Out, A] {
  def completeIn(a: A, in: In): F[Response]
}

trait Complete[F[_], R, A] extends CompleteIn[F, Any, R, A] {
  def complete(a: A): F[Response]

  def completeIn(a: A, in: Any): F[Response] = complete(a)
}

object Complete {
  implicit def contravariant[F[_], R]: Contravariant[Complete[F, R, *]] =
    new Contravariant[Complete[F, R, *]] {
      def contramap[A, B](fa: Complete[F, R, A])(f: B => A): Complete[F, R, B] = b => fa.complete(f(b))
    }
}

final case class Rejection(
    priority:     Double,
    status:       http.Status = NotFound,
    path:         String = "",
    wrongMethod:  List[String] = Nil,
    missing:      List[MissingParam] = Nil,
    malformed:    List[MalformedParam] = Nil,
    unauthorized: Boolean = false,
    messages:     List[String] = Nil
) {
  def withPath(p:   String): Rejection = copy(path = p)
  def addMessage(s: String): Rejection = copy(messages = s :: messages)
  def message: String = messages.headOption.getOrElse {
    (Iterator(s"at $path") ++
      Iterator(
        "incorrect methods" -> wrongMethod,
        "missing parameters" -> missing.map(p => s"${p.name} in ${p.source}"),
        "malformed parameters" -> malformed.map(p => s"${p.name} in ${p.source} with ${p.error}")
      ).collect { case (m, ms) if ms.nonEmpty => ms.mkString(m, ",", "") } ++
      Iterator("unauthorized").filter(_ => unauthorized)).mkString("\n")
  }
}

object Rejection {
  val notFound = Rejection(0)
  def wrongMethod(method:  String) = Rejection(1, MethodNotAllowed, wrongMethod = List(method))
  def missingParam(name:   String, source: ParamSource) = Rejection(3, BadRequest, missing = List(MissingParam(name, source)))
  def malformedParam(name: String, error: String, source: ParamSource) =
    Rejection(4, BadRequest, malformed = List(MalformedParam(name, error, source)))
  def body(error: String) = Rejection(5, BadRequest, messages = List(error))
  val unauthorized = Rejection(6, Unauthorized)
  case class MissingParam(name:   String, source: ParamSource)
  case class MalformedParam(name: String, error:  String, source: ParamSource)

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

  type Handler = Rejection => Response

  val defaultHandler: Handler = rej => {
    val response = http.Response(rej.status)
    response.setContentString(rej.message)
    response
  }
}

trait ConvertService[F[_]] {
  def convertService[A](svc: Service[http.Request, A]): F[A]
}

class RoutedFunctions {
  def matched[F[_]](implicit routed:              Routed[F]): F[Int] = routed.matched
  def path[F[_]](implicit routed:                 Routed[F]): F[CharSequence] = routed.path
  def withMatched[F[_], A](m:                     Int, fa: F[A])(implicit routed: Routed[F]): F[A] = routed.withMatched(m, fa)
  def request[F[_]](implicit routed:              Routed[F]): F[Request] = routed.request
  def matchedPath[F[_]: Apply](implicit routed:   Routed[F]): F[CharSequence] = (path, matched).mapN(_.subSequence(0, _))
  def unmatchedPath[F[_]: Apply](implicit routed: Routed[F]): F[CharSequence] =
    (path, matched).mapN((p, m) => p.subSequence(m, p.length()))
  def reject[F[_], A](rejection: Rejection)(implicit routed: Routed[F]): F[A] = routed.reject(rejection)

  def rejectMany[F[_]: Routed, A](rejections: Rejection*): F[A] = reject(Foldable[List].fold(rejections.toList))
  def addMatched[F[_]: Monad: Routed, A](i:   Int, fa: F[A]): F[A] = matched >>= (m => withMatched(m + i, fa))
  def segment[F[_]: Routed: Monad, A](f:      Option[CharSequence] => F[A]): F[A] = customSegment(SegmentPattern, 1, f)

  def checkPrefix[F[_]: Monad: Routed, A](pref: CharSequence, fa: F[A]): F[A] =
    for {
      path <- unmatchedPath[F]
      i = commonPathPrefix(path, pref)
      res <- if (i < 0) reject[F, A](notFound) else if (i > 0) addMatched(i, fa) else fa
    } yield res

  def checkPath[F[_]: Monad: Routed, A](full: CharSequence, fa: F[A]): F[A] =
    for {
      path <- unmatchedPath[F]
      i = commonPathPrefix(path, full)
      res <- if (i == path.length()) addMatched(i, fa) else reject[F, A](notFound)
    } yield res

  def checkPathEnd[F[_]: Monad: Routed, A](fa: F[A]): F[A] = checkPath("", fa)

  private[this] val SegmentPattern = "/?([^/]*)".r

  def customSegment[F[_]: Monad: Routed, A](pattern: Regex, groupId: Int, f: Option[CharSequence] => F[A]): F[A] =
    for {
      path <- unmatchedPath[F]
      matchOpt = pattern.findPrefixMatchOf(path)
      res <- matchOpt.fold(f(None))(m => addMatched(m.end - m.start, f(Some(m.group(groupId)))))
    } yield res

  def param[F[_]: Routed: Monad, S >: ParamSource.All <: ParamSource, T](name:            String)(implicit param: Param[S, T],
                                                                                       p: ParamDirectives[S]): F[T] =
    p.getByName[F, T](name, os => p.provideOrReject[F, T](name, param.get(name, _ => os)))

  def uriParam[F[_]: Routed: Monad, T: Param.PQuery](name:     String) = param[F, ParamSource.Query, T](name)
  def formParam[F[_]: Routed: Monad, T: Param.PForm](name:     String) = param[F, ParamSource.Form, T](name)
  def headerParam[F[_]: Routed: Monad, T: Param.PHeader](name: String) = param[F, ParamSource.Header, T](name)
  def cookieParam[F[_]: Routed: Monad, T: Param.PCookie](name: String) = param[F, ParamSource.Cookie, T](name)

  private[this] def commonPathPrefix(path: CharSequence, pref: CharSequence): Int =
    if (pref.length() <= path.length() && path.subSequence(0, pref.length()) == pref) pref.length()
    else if (pref.length() < path.length() && path.charAt(0) == '/' && path.subSequence(1, pref.length() + 1) == pref)
      pref.length() + 1
    else -1
}
