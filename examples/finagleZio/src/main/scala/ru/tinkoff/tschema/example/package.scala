package ru.tinkoff.tschema

import ru.tinkoff.tschema.finagle.LiftHttp
import ru.tinkoff.tschema.finagle.zioRouting._
import zio._
import zio.blocking.Blocking
import zio.console.Console

package object example {
  type FullEnv = Console with Blocking with HasExample
  type Http[+A] = URIOH[FullEnv, A]
  type HasExample = Has[ExampleEnv]
  type Example[+A] = URIO[FullEnv, A]
}
