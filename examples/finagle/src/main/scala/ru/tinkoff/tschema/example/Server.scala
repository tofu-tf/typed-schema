package ru.tinkoff.tschema.example

import Swagger._
import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import com.twitter.finagle.Http
import com.twitter.finagle.http.Response
import com.twitter.util.{Await, Duration}
import ru.tinkoff.tschema.finagle.Runnable
import zio.console._
import zio.{blocking => _, _}

object Server extends App {
  val modules: List[ExampleModule] = List(Greeting, TestModule, FiltersModule)

  val svc: Http[Response] = modules.foldMapK(_.route) <+> Swagger.route

  val server = for {
    srv <- Runnable.run[Example](svc)
    list <- ZIO.effect(Http.serve("0.0.0.0:9191", srv))
    _ <- putStr(s"started at ${list.boundAddress}")
    _ <- ZIO.effect(Await.ready(list, Duration.Top)).fork
    res <- ZIO.never
  } yield res

  def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    for {
      ref <- Ref.make(0)
      _ <- server
        .catchAll(ex => putStr(ex.getMessage))
        .provide(ExampleEnv("lol", ref))
    } yield 0
}
