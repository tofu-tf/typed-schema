package ru.tinkoff.tschema
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.finagle.zioInstance._
import scalaz.zio._

package object example {
  type Http[+A]    = ZIOHttp[ExampleEnv, Nothing, A]
  type Example[+A] = ZIO[ExampleEnv, Nothing, A]
}
