package ru.tinkoff.tschema.finagle.routing

import cats.syntax.semigroup._
import com.twitter
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.finagle.{Service, http}
import com.twitter.util.{Future, Promise}
import monix.eval.Task
import monix.execution.Scheduler
import ru.tinkoff.tschema.finagle.routing.TaskRouting.TaskHttp
import ru.tinkoff.tschema.finagle.{ConvertService, LiftHttp, Rejection, Routed, RoutedPlus, RunHttp}
import ru.tinkoff.tschema.utils.SubString
import tofu.env.Env

final case class TaskRouting(
    request: http.Request,
    path: CharSequence,
    matched: Int
)

object TaskRouting extends TaskInstanceDecl {

  type TaskHttp[+A] = Env[TaskRouting, A]

  implicit val taskRouted: RoutedPlus[TaskHttp] with ConvertService[TaskHttp] with LiftHttp[TaskHttp, Task] =
    new TaskRoutedConvert

  implicit def envRunnable(
      implicit rejectionHandler: Rejection.Handler = Rejection.defaultHandler
  ): RunHttp[TaskHttp, Task] =
    response => Task.deferAction(implicit sched => Task.delay(execResponse(response, _)))

  private[this] def execResponse(
      envResponse: TaskHttp[Response],
      request: Request
  )(implicit sc: Scheduler, handler: Rejection.Handler): Future[Response] = {
    val promise = Promise[Response]
    val routing = TaskRouting(request, SubString(request.path), 0)

    val cancelable = envResponse.run(routing).onErrorRecover { case Rejected(rej) => handler(rej) }.runAsync {
      case Right(res) => promise.setValue(res)
      case Left(ex) =>
        val resp = Response(Status.InternalServerError)
        resp.setContentString(ex.getMessage)
        promise.setValue(resp)
    }

    promise.setInterruptHandler { case _ => cancelable.cancel() }

    promise
  }
}

private[finagle] class TaskInstanceDecl {

  protected class TaskRoutedConvert
      extends RoutedPlus[TaskHttp] with ConvertService[TaskHttp] with LiftHttp[TaskHttp, Task] {
    private type F[a] = TaskHttp[a]
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
      Env { r =>
        Task.cancelable { cb =>
          val fut = svc(r.request).respond {
            case twitter.util.Return(a) => cb.onSuccess(a)
            case twitter.util.Throw(ex) => cb.onError(ex)
          }

          Task(fut.raise(new InterruptedException))
        }
      }

    def apply[A](fa: Task[A]): TaskHttp[A] = Env.fromTask(fa)

    @inline private[this] def catchRej[A](z: F[A])(f: Rejection => F[A]): F[A] =
      z.onErrorRecoverWith { case Rejected(xrs) => f(xrs) }

    @inline private[this] def throwRej[A](map: Rejection): F[A] = Env.raiseError(Rejected(map))
  }

}
