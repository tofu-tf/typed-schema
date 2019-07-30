package ru.tinkoff.tschema

import ru.tinkoff.tschema.finagle.routing._
import zio._

package object example {
  type Http[+A]    = ZioRouting.ZIOHttp[ExampleEnv, Nothing, A]
  type Example[+A] = ZIO[ExampleEnv, Nothing, A]

}
