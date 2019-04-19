package ru.tinkoff.tschema.example
import com.twitter.finagle.{Http, Service}
import ru.tinkoff.tschema.finagle.{Routed, Runnable}
import scalaz.zio._
import scalaz.zio.console._
import ru.tinkoff.tschema.finagle.zioInstance._
import com.twitter.conversions.DurationOps._
import com.twitter.finagle.http.{Request, Response, Status, Version}
import com.twitter.io.{Buf, Reader}
import com.twitter.util.{Await, Duration, Future}

object Server extends App {
  val server = for {
    srv  <- Runnable.run[Exec](Greeting.svc)
    list <- ZIO.effect(Http.serve("0.0.0.0:9191", srv))
    _    <- putStr(s"started at ${list.boundAddress}")
    _    <- ZIO.effect(Await.ready(list, Duration.Top)).fork
    res  <- ZIO.never
  } yield res

  def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    for {
      ref <- Ref.make(0)
      _   <- server.catchAll(ex => putStr(ex.getMessage)).provide(Example("lol", ref))
    } yield 0
}

final case class Example(trackingId: String, alohas: Ref[Int]) extends Console.Live

object Example {
  final implicit val httpRouted: Routed[Http]      = zioRouted
  final implicit val httpRun: Runnable[Http, Exec] = zioRunnable()

  val incrementAlohas: Http[Int] = ZIO.accessM(_.embedded.alohas.update(_ + 1))
}
