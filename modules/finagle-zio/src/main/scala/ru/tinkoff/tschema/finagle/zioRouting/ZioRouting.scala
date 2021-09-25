package ru.tinkoff.tschema.finagle.zioRouting

import cats.evidence.As
import com.twitter.finagle.http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Future
import ru.tinkoff.tschema.finagle.Rejection.Recover
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.finagle.zioRouting.impl._
import ru.tinkoff.tschema.utils.SubString
import zio.{Has, URIO, ZIO}
import zio.interop.catz.monadErrorInstance

final case class ZioRouting[@specialized(Unit) +R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
)

object ZRouting {
  def apply(request: Request, path: CharSequence, matched: Int): ZRouting =
    ZioRouting(request, path, matched, ())

  def unapply(x: Any): Option[(http.Request, CharSequence, Int)] = x match {
    case ZioRouting(request, path, matched, ()) => Some((request, path, matched))
    case _                                      => None
  }
}

object ZioRouting extends ZioRoutingInstances {}

/** trait containing implicits specifically for case E = Nothing since scala 2 compiler has issues with Nothing
  */
trait ZioRoutingInstances extends ZioRoutingInstances2 { self: ZioRouting.type =>

  final implicit def zioRoutedU[R]: RoutedPlus[URIOHttp[R, *]] = zioRouted[R, Nothing]

  final implicit def zioLiftU[R, R1](implicit evr: R <:< R1): LiftHttp[URIOHttp[R, *], URIO[R1, *]] =
    zioLift[R, R1, NoError, Nothing]
  final implicit def zioConvertServiceU[R]: ConvertService[URIOHttp[R, *]]                          = zioConvertService[R, NoError]
  final implicit def zioRunnableU[R](implicit
      recover: Recover[URIOHttp[R, *]] = Recover.default[URIOHttp[R, *]]
  ): RunHttp[URIOHttp[R, *], URIO[R, *]] = zioRunnable[R, NoError]

  implicit def ziosRoutedU[R]: RoutedPlus[URIOH[R, *]]                                              = ziosRouted[R, Nothing]
  implicit def ziosLiftU[R, R1](implicit asR: R As R1): LiftHttp[URIOH[R, *], URIO[R1, *]]          =
    ziosLift[R, R1, Nothing, Nothing]
  implicit def ziosConvertServiceU[R]: ConvertService[URIOH[R, *]]                                  =
    ziosConvertService[R, Nothing]
  implicit def ziosRunnableU[R <: Has[_]](implicit
      recover: Recover[URIOH[R, *]] = Recover.default[URIOH[R, *]]
  ): RunHttp[URIOH[R, *], URIO[R, *]] =
    ziosRunnable[R, Nothing]
}

trait ZioRoutingInstances2 {
  implicit def zioRouted[R, E]: RoutedPlus[ZIOHttp[R, E, *]]                                                         =
    zioRoutedAny.asInstanceOf[ZioRoutedInstance[R, E]]

  implicit def zioLift[R, R1, E, E1](implicit
      eve: E1 <:< E,
      evr: R <:< R1
  ): LiftHttp[ZIOHttp[R, E, *], ZIO[R1, E1, *]] =
    zioLiftAny.asInstanceOf[ZioLiftInstance[R, R1, E, E1]]

  implicit def zioConvertService[R, E]: ConvertService[ZIOHttp[R, E, *]]                                             =
    zioConvertServiceAny.asInstanceOf[ConvertService[ZIOHttp[R, E, *]]]

  implicit def zioRunnable[R, E <: Throwable](implicit
      recover: Recover[ZIO[ZioRouting[R], E, *]] = Recover.default[ZIO[ZioRouting[R], E, *]]
  ): RunHttp[ZIOHttp[R, E, *], ZIO[R, E, *]] =
    zioResponse => ZIO.runtime[R].flatMap(runtime => ZIO.effectTotal(exec(runtime, zioResponse, _)))

  implicit def ziosRouted[R, E]: RoutedPlus[ZIOH[R, E, *]]                                                           =
    ziosRoutedAny.asInstanceOf[ZiosRoutedInstance[R, E]]

  implicit def ziosLift[R, R1, E, E1](implicit asE: E1 <:< E, asR: R As R1): LiftHttp[ZIOH[R, E, *], ZIO[R1, E1, *]] =
    ziosLiftAny.asInstanceOf[ZiosLiftInstance[R, R1, E, E1]]

  implicit def ziosConvertService[R, E]: ConvertService[ZIOH[R, E, *]]                                               =
    ziosConvertAny.asInstanceOf[ConvertService[ZIOH[R, E, *]]]

  implicit def ziosRunnable[R <: Has[_], E <: Throwable](implicit
      recover: Recover[ZIO[R with HasRouting, E, *]] = Recover.default[ZIO[R with HasRouting, E, *]]
  ): RunHttp[ZIOH[R, E, *], ZIO[R, E, *]] = zioResponse =>
    ZIO.runtime[R].flatMap(runtime => ZIO.effectTotal(execs(runtime, zioResponse, _)))

  private def catchRejections[R, E, A](r: ZIO[R, Fail[E], Response])(implicit recover: Recover[ZIO[R, E, *]])        =
    r.catchAll {
      case Fail.Rejected(rejection) => recover(rejection)
      case Fail.Other(e)            => ZIO.fail(e)
    }

  private[this] def execs[R <: Has[_], E <: Throwable](
      runtime: zio.Runtime[R],
      zioResponse: ZIOH[R, E, Response],
      request: Request
  )(implicit recover: Recover[ZIO[R with HasRouting, E, *]]): Future[Response] =
    execResponse[R, R with HasRouting, E](
      runtime,
      catchRejections(zioResponse),
      _ add ZRouting(request, SubString(request.path), 0)
    )

  private[this] def exec[R, E <: Throwable](
      runtime: zio.Runtime[R],
      zioResponse: ZIOHttp[R, E, Response],
      request: Request
  )(implicit recover: Recover[ZIO[ZioRouting[R], E, *]]): Future[Response] = {
    execResponse[R, ZioRouting[R], E](
      runtime,
      catchRejections(zioResponse),
      ZioRouting(request, SubString(request.path), 0, _)
    )
  }

  private[this] val zioRoutedAny                                                                                     = new ZioRoutedInstance[Any, Nothing]
  private[this] val zioLiftAny                                                                                       = new ZioLiftInstance[Any, Any, Nothing, Nothing]
  private[this] val zioConvertServiceAny                                                                             = new ZIOConvertService[Any, Nothing]

  private[this] val ziosRoutedAny  = new ZiosRoutedInstance[Any, Nothing]
  private[this] val ziosLiftAny    = new ZiosLiftInstance[Any, Any, Nothing, Nothing]
  private[this] val ziosConvertAny = new ZiosConvertService[Any, Nothing]
}
