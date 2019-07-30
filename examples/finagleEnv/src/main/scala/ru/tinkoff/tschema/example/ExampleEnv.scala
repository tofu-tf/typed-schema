package ru.tinkoff.tschema.example

import cats.Monad
import cats.effect.concurrent.Ref
import monix.eval.Task
import ru.tinkoff.tschema.finagle.EnvRouting
import tofu.env.EnvSpecializedFunctions

final case class ExampleEnv(trackingId: String, alohas: Ref[Task, Int])

object ExampleEnv {
  val incrementAlohas: Example[Int] = Example(_.alohas.modify(i => (i + 1, i + 1)))
}

object Example extends EnvSpecializedFunctions[ExampleEnv]
object Http    extends EnvSpecializedFunctions[EnvRouting[ExampleEnv]]
