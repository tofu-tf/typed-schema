package ru.tinkoff.tschema

import cats.Monad
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.finagle.zioInstance._
import zio._

package object example {
  type Http[+A] = ZIOHttp[ExampleEnv, Nothing, A]
  type Example[+A] = ZIO[ExampleEnv, Nothing, A]

  implicit val runnable: Runnable[Http, Example] = zioRunnable()
  implicit val httpMonad: Monad[Http] = zio.interop.catz.ioInstances
  implicit val exampleMonad: Monad[Example] = zio.interop.catz.ioInstances
  implicit val routed: RoutedPlus[Http] = zioRouted
}
