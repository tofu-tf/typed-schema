package ru.tinkoff.tschema.example
import ru.tinkoff.tschema.finagle.{Routed, Runnable}
import ru.tinkoff.tschema.finagle.zioInstance._
import scalaz.zio.console._
import scalaz.zio.{Ref, ZIO}

final case class ExampleEnv(trackingId: String, alohas: Ref[Int]) extends Console.Live

object ExampleEnv {
  final implicit val httpRouted: Routed[Http]         = zioRouted
  final implicit val httpRun: Runnable[Http, Example] = zioRunnable()

  val incrementAlohas: Http[Int] = ZIO.accessM(_.embedded.alohas.update(_ + 1))
}
