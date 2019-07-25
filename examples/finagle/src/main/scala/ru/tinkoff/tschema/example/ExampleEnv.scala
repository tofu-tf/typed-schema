package ru.tinkoff.tschema.example

import ru.tinkoff.tschema.finagle.{Routed, Runnable}
import ru.tinkoff.tschema.finagle.zioInstance._
import zio.blocking.Blocking
import zio.console._
import zio.{Ref, ZIO}

final case class ExampleEnv(trackingId: String, alohas: Ref[Int])
    extends Console.Live
    with Blocking.Live

object ExampleEnv {
  final implicit val httpRouted: Routed[Http] = zioRouted
  final implicit val httpRun: Runnable[Http, Example] = zioRunnable()

  val incrementAlohas: Example[Int] = ZIO.accessM(_.alohas.update(_ + 1))
}
