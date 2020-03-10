package ru.tinkoff.tschema.example

import ExampleSwagger._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import com.twitter.finagle.Http
import com.twitter.finagle.http.Response
import com.twitter.util.{Await, Duration}
import ru.tinkoff.tschema.finagle.RunHttp
import zio.console._
import zio.{blocking => _, _}

object Server extends App {
  val modules: List[ExampleModule] =
    List(
      Greeting,
      TestModule,
      FiltersModule,
      FormFieldsModule,
      MultiParameters,
      ProxyModule,
      VersionModule,
      Authorize,
      ReceiveModule
    )

  val svc: Http[Response] = modules.foldMapK(_.route) <+> ExampleSwagger.route

  val server = for {
    srv <- RunHttp.run[Example](svc)
    list <- ZIO.effect(Http.serve("0.0.0.0:9191", srv))
    _ <- putStr(s"started at ${list.boundAddress}")
    _ <- ZIO.effect(Await.ready(list, Duration.Top)).fork
    res <- ZIO.never
  } yield res

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    for {
      ref <- Ref.make(0)
      storage <- Ref.make(Map[String, String]())
      _ <- server
        .catchAll(ex => putStr(ex.getMessage))
        .provide(ExampleEnv("lol", ref, storage))
    } yield 0
}
