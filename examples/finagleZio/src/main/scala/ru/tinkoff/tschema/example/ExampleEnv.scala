package ru.tinkoff.tschema.example

import cats.Monad
import zio.blocking.Blocking
import zio.console._
import zio.{Ref, ZIO}

final case class ExampleEnv(trackingId: String, alohas: Ref[Int])
    extends Console.Live
    with Blocking.Live

object ExampleEnv {
  val incrementAlohas: Example[Int] = ZIO.accessM(_.alohas.update(_ + 1))

  final implicit val exampleMonad: Monad[Example] = zio.interop.catz.ioInstances

  final implicit val httpMonad: Monad[Http] = zio.interop.catz.ioInstances
}
