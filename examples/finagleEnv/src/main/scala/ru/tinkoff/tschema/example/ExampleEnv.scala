package ru.tinkoff.tschema.example

import cats.effect.concurrent.Ref
import monix.eval.Task
import ru.tinkoff.tschema.finagle.envRouting._
import simulacrum.typeclass
import tofu.env.EnvSpecializedFunctions

final case class ExampleEnv(trackingId: String, alohas: Ref[Task, Int])

object ExampleEnv {
  implicit val exampleAlohas: Alohas[Example] = () => Example(_.alohas.modify(i => (i + 1, i + 1)))
}

object Example extends EnvSpecializedFunctions[ExampleEnv]
object Http    extends EnvSpecializedFunctions[EnvRouting[ExampleEnv]]

@typeclass trait Alohas[F[_]] {
  def increment(): F[Int]
}
