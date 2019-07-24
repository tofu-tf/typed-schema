package ru.tinkoff.tschema.example

import com.twitter.finagle.Http
import com.twitter.util.{Await, Duration}
import ru.tinkoff.tschema.finagle.Runnable
import zio._
import zio.console._
import ru.tinkoff.tschema.finagle.zioInstance._

object Server extends App {
  implicit val runnable: Runnable[Http, Example] = zioRunnable()

  val server = for {
    srv <- Runnable.run[Example](Greeting.svc)
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
