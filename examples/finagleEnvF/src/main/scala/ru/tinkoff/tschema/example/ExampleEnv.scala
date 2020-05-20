package ru.tinkoff.tschema.example

import cats.effect.concurrent.Ref
import monix.eval.Task
import ru.tinkoff.tschema.finagle.routing._
import tofu.env.EnvSpecializedFunctions

final case class ExampleEnv(trackingId: String, alohas: Ref[Task, Int])

trait AlohasState[F[_]] {
  def incrementAlohas(): F[Int]
}

object ExampleEnv {
  implicit val alohasState: AlohasState[Example] =
    () => Example(_.alohas.modify(i => (i + 1, i + 1)))
}

object Example extends EnvSpecializedFunctions[ExampleEnv]
object Http    extends EnvSpecializedFunctions[EnvRouting[ExampleEnv]]
