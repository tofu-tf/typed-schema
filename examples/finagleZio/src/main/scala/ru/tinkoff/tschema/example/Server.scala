package ru.tinkoff.tschema.example

import ExampleSwagger._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import com.twitter.finagle.{Http => FHttp}
import com.twitter.finagle.http.Response
import com.twitter.util.{Await, Duration}
import ru.tinkoff.tschema.finagle.RunHttp
import zio.blocking.Blocking
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
    list <- ZIO.effect(FHttp.serve("0.0.0.0:9191", srv))
    _ <- putStr(s"started at ${list.boundAddress}")
    _ <- ZIO.effect(Await.ready(list, Duration.Top)).fork
    res <- ZIO.never
  } yield res

  val layer: URLayer[Blocking with Console, FullEnv] =
    ZLayer.identity[Blocking with Console].zipWithPar(ExampleEnv.live)(_ union _)

  def run(args: List[String]): URIO[Blocking with Console, Int] =
    layer.build.use(r => server.catchAll(ex => putStr(ex.getMessage)).provide(r)) as 0
}
