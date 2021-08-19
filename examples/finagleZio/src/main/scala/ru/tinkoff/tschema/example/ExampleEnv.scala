package ru.tinkoff.tschema.example

import cats.Monad
import zio._

final case class ExampleEnv(
    trackingId: String,
    alohas: Ref[Int],
    storage: Ref[Map[String, String]]
)

object ExampleEnv {
  val live: ULayer[HasExample] = ZLayer.fromEffect(
    for {
      ref     <- Ref.make(0)
      storage <- Ref.make(Map[String, String]())
    } yield ExampleEnv("lol", ref, storage)
  )

  final implicit val exampleMonad: Monad[Example] = zio.interop.catz.monadErrorInstance

  final implicit val httpMonad: Monad[Http] = zio.interop.catz.monadErrorInstance
}

object Example {
  val incrementAlohas: URIO[HasExample, Int]              = ZIO.accessM(_.get.alohas.updateAndGet(_ + 1))
  val storage: URIO[HasExample, Ref[Map[String, String]]] = ZIO.access(_.get.storage)
}
