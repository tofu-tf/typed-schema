package ru.tinkoff.tschema

import ru.tinkoff.tschema.finagle.LiftHttp
import ru.tinkoff.tschema.finagle.routing._
import zio._

package object example {
  type Http[+A] = ZioRouting.URIOHttp[ExampleEnv, A]
  type Example[+A] = URIO[ExampleEnv, A]
}
