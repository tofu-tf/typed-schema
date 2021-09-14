package ru.tinkoff.tschema.example

import cats.effect.ExitCode
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import com.twitter.finagle
import com.twitter.finagle.http.Response
import com.twitter.util.{Await, Duration}
import monix.eval.{Task, TaskApp}
import ru.tinkoff.tschema.example.sample.SampleModule
import ru.tinkoff.tschema.finagle.RunHttp
import tofu.Void

object Server extends TaskApp {
  def modules[H[_]]: List[ExampleModule[Http]] =
    List(
      new Greeting[Http, Example](),
      new SampleModule[Http, Example](),
      new FiltersModule(),
      new FormFieldsModule(),
      new MultiParameters(),
      new ProxyModule(),
      new VersionModule(),
      new Authorize
    )

  val svc: Http[Response] = modules.foldMapK(_.route) <+> ExampleSwagger.route

  val server = for {
    srv  <- RunHttp.run[Example](svc)
    list <- Example.delay(finagle.Http.serve("0.0.0.0:9191", srv))
    _    <- Example.delay(println(s"started at ${list.boundAddress}"))
    _    <- Example.delay(Await.ready(list, Duration.Top)).fork
    res  <- Example.fromTask(Task.never[Void])
  } yield res

  def run(args: List[String]): Task[ExitCode] =
    for {
      ref <- Ref[Task].of(0)
      _   <- server
               .onErrorHandle(ex => println(ex.getMessage))
               .run(ExampleEnv("lol", ref))
    } yield ExitCode.Success
}
