package ru.tinkoff.tschema.example

import cats.Monad
import ru.tinkoff.tschema.finagle.{Routed, RoutedPlus, Runnable}
import ru.tinkoff.tschema.finagle.zioInstance._
import zio.blocking.Blocking
import zio.console._
import zio.{Ref, ZIO}

final case class ExampleEnv(trackingId: String, alohas: Ref[Int])
    extends Console.Live
    with Blocking.Live

object ExampleEnv {
  final implicit val httpRun: Runnable[Http, Example] = zioRunnable()

  val incrementAlohas: Example[Int] = ZIO.accessM(_.alohas.update(_ + 1))

  implicit val exampleMonad: Monad[Example] = zio.interop.catz.ioInstances

  final implicit val httpRouted: RoutedPlus[Http] = zioRouted

  implicit val httpMonad: Monad[Http] = zio.interop.catz.ioInstances
}
