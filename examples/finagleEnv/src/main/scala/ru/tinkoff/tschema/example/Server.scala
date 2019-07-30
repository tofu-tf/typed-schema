package ru.tinkoff.tschema.example

import Swagger._
import cats.effect.ExitCode
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import com.twitter.finagle
import finagle.{Http, http}
import com.twitter.finagle.http.Response
import com.twitter.util.{Await, Duration}
import monix.eval.{Task, TaskApp}
import ru.tinkoff.tschema.finagle.Runnable
import tofu.Void

object Server extends TaskApp {
  val modules: List[ExampleModule] =
    List(Greeting, TestModule, FiltersModule, FormFieldsModule, MultiParameters, ProxyModule, VersionModule, Authorize)

  val svc: Http[Response] = modules.foldMapK(_.route) <+> Swagger.route

  val server = for {
    srv  <- Runnable.run[Example](svc)
    list <- Example.delay(finagle.Http.serve("0.0.0.0:9191", srv))
    _    <- Example.delay(println(s"started at ${list.boundAddress}"))
    _    <- Example.delay(Await.ready(list, Duration.Top)).fork
    res  <- Example.fromTask(Task.never[Void])
  } yield res

  def run(args: List[String]): Task[ExitCode] =
    for {
      ref <- Ref[Task].of(0)
      _ <- server
            .onErrorHandle(ex => println(ex.getMessage))
            .run(ExampleEnv("lol", ref))
    } yield ExitCode.Success
}
