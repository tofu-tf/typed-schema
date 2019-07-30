package ru.tinkoff.tschema.finagle

import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import monix.eval.Task
import monix.execution.Scheduler
import ru.tinkoff.tschema.finagle.EnvRouting.EnvHttp
import ru.tinkoff.tschema.finagle.ReaderTRouting.ReaderHttp
import ru.tinkoff.tschema.utils.SubString
import tofu.env.Env

final case class EnvRouting[+R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
)

object EnvRouting extends EnvInstanceDecl {

  type EnvHttp[R, +A] = Env[ReaderTRouting[R], A]

  implicit def zioRouted[R, E]
    : RoutedPlus[EnvHttp[R, *]] with ConvertService[EnvHttp[R, *]] with LiftHttp[EnvHttp[R, *], Env[R, *]] =
    envRoutedAny.asInstanceOf[EnvRoutedConvert[R]]

  implicit def envRunnable[R](
      implicit rejectionHandler: Rejection.Handler = Rejection.defaultHandler): Runnable[EnvHttp[R, *], Env[R, *]] =
    zioResponse => Env(r => Task.deferAction(implicit sched => Task.delay(execResponse(r, zioResponse, _))))

  private[this] def execResponse[R](r: R, envResponse: EnvHttp[R, Response], request: Request)(
      implicit sc: Scheduler,
      handler: Rejection.Handler): Future[Response] = {
    val promise = Promise[Response]
    val routing = ReaderTRouting(request, SubString(request.path), 0, r)

    envResponse.run(routing).onErrorRecover { case Rejected(rej) => handler(rej) }.runAsync {
      case Right(res) => promise.setValue(res)
      case Left(ex) =>
        val resp = Response(Status.InternalServerError)
        resp.setContentString(ex.getMessage)
        promise.setValue(resp)
    }

    promise
  }
}

private[finagle] class EnvInstanceDecl {

  final case class Rejected(rej: Rejection) extends Throwable

  protected trait EnvRoutedConvert[R]
      extends RoutedPlus[EnvHttp[R, *]] with ConvertService[EnvHttp[R, *]] with LiftHttp[EnvHttp[R, *], Env[R, *]] {
    private type F[a] = EnvHttp[R, a]
    implicit private[this] val self: RoutedPlus[F] = this

    def matched: F[Int] = Env.fromFunc(_.matched)

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.local(_.copy(matched = m))

    def path: F[CharSequence]    = Env.fromFunc(_.path)
    def request: F[http.Request] = Env.fromFunc(_.request)
    def reject[A](rejection: Rejection): F[A] =
      Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

    def combineK[A](x: F[A], y: F[A]): F[A] =
      catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))

    def convertService[A](svc: Service[http.Request, A]): F[A] =
      Env(r =>
        Task.async(cb =>
          svc(r.request).respond {
            case twitter.util.Return(a) => cb.onSuccess(a)
            case twitter.util.Throw(ex) => cb.onError(ex)
        }))

    def apply[A](fa: Env[R, A]): EnvHttp[R, A] = fa.localP(_.embedded)
  }

  protected object envRoutedAny extends EnvRoutedConvert[Any]

  @inline private[this] def catchRej[R, A](z: EnvHttp[R, A])(f: Rejection => EnvHttp[R, A]): EnvHttp[R, A] =
    z.onErrorRecoverWith { case Rejected(xrs) => f(xrs) }

  @inline private[this] def throwRej[R, A](map: Rejection): EnvHttp[R, A] = Env.raiseError(Rejected(map))

}
