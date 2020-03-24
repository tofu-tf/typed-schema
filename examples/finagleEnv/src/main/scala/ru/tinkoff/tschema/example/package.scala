package ru.tinkoff.tschema

import monix.execution.Scheduler
import ru.tinkoff.tschema.finagle.envRouting._
import tofu.env.Env

package object example {
  type Http[+A]    = EnvRouting.EnvHttp[ExampleEnv, A]
  type Example[+A] = Env[ExampleEnv, A]

  val resources = Scheduler.io()
}
