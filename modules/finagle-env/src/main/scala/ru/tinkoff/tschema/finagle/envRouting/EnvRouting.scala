package ru.tinkoff.tschema.finagle.envRouting

import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import monix.eval.Task
import monix.execution.Scheduler
import ru.tinkoff.tschema.finagle.Rejection.Recover
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.finagle.envRouting.EnvRouting.EnvHttp
import ru.tinkoff.tschema.utils.SubString
import tofu.env.Env

final case class EnvRouting[+R](
    request: http.Request,
    path: CharSequence,
    matched: Int,
    embedded: R
)

object EnvRouting extends EnvInstanceDecl {

  type EnvHttp[R, +A] = Env[EnvRouting[R], A]

  implicit def taskRouted[R]
      : RoutedPlus[EnvHttp[R, *]] with ConvertService[EnvHttp[R, *]] with LiftHttp[EnvHttp[R, *], Env[R, *]] =
    envRoutedAny.asInstanceOf[EnvRoutedConvert[R]]

  implicit def envRunnable[R](implicit
      recover: Recover[EnvHttp[R, *]] = Recover.default[EnvHttp[R, *]]
  ): RunHttp[EnvHttp[R, *], Env[R, *]] =
    response => {
      val handled = response.onErrorRecoverWith { case Rejected(rej) => recover(rej) }
      Env(r => Task.deferAction(implicit sched => Task.delay(execResponse(r, handled, _))))
    }

  private[this] def execResponse[R](r: R, envResponse: EnvHttp[R, Response], request: Request)(implicit
      sc: Scheduler
  ): Future[Response] = {
    val promise = Promise[Response]()
    val routing = EnvRouting(request, SubString(request.path), 0, r)

    val cancelable = envResponse.run(routing).runAsync {
      case Right(res) => promise.setValue(res)
      case Left(ex)   =>
        val resp    = Response(Status.InternalServerError)
        val message = Option(ex.getLocalizedMessage).getOrElse(ex.toString)
        resp.setContentString(message)
        promise.setValue(resp)
    }

    promise.setInterruptHandler { case _ => cancelable.cancel() }

    promise
  }
}

private[finagle] class EnvInstanceDecl {

  protected trait EnvRoutedConvert[R]
      extends RoutedPlus[EnvHttp[R, *]] with ConvertService[EnvHttp[R, *]] with LiftHttp[EnvHttp[R, *], Env[R, *]] {
    private type F[a] = EnvHttp[R, a]
    implicit private[this] val self: RoutedPlus[F] = this

    def matched: F[Int] = Env.fromFunc(_.matched)

    def withMatched[A](m: Int, fa: F[A]): F[A] = fa.local(_.copy(matched = m))

    def path: F[CharSequence]                 = Env.fromFunc(_.path)
    def request: F[http.Request]              = Env.fromFunc(_.request)
    def reject[A](rejection: Rejection): F[A] =
      Routed.unmatchedPath[F].flatMap(path => throwRej(rejection withPath path.toString))

    def combineK[A](x: F[A], y: F[A]): F[A] =
      catchRej(x)(xrs => catchRej(y)(yrs => throwRej(xrs |+| yrs)))

    def convertService[A](svc: Service[http.Request, A]): F[A] =
      Env { r =>
        Task.cancelable { cb =>
          val fut = svc(r.request).respond {
            case twitter.util.Return(a) => cb.onSuccess(a)
            case twitter.util.Throw(ex) => cb.onError(ex)
          }

          Task(fut.raise(new InterruptedException))
        }
      }

    def apply[A](fa: Env[R, A]): EnvHttp[R, A]                                 = fa.localP(_.embedded)
    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.onErrorRecoverWith { case Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] = Env.raiseError(envRouting.Rejected(map))
  }

  protected object envRoutedAny extends EnvRoutedConvert[Any]
}
