package ru.tinkoff.tschema
import ru.tinkoff.tschema.finagle._
import ru.tinkoff.tschema.finagle.zioInstance._
import scalaz.zio._

package object example {
  type Http[+A] = ZIOHttp[Example, Nothing, A]
  type Exec[+A] = ZIO[Example, Nothing, A]
}
