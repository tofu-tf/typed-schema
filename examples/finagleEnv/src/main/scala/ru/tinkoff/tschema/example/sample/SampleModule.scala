package ru.tinkoff.tschema.example
package sample

import cats.Monad
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.finagle.{LiftHttp, RoutedPlus}
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._

class SampleModule[H[_]: Monad: RoutedPlus: LiftHttp[*[_], F], F[_]: Monad: SampleOps: SampleString]
    extends ExampleModule[H] {
  import SampleModule._

  val swag = MkSwagger(api)

  val route = MkService[H](api)(new handler[F])
}

object SampleModule {

  trait Mutate {
    def mutate(value: Long) = java.lang.Long.toBinaryString(value)
    def concat(l:     String, r: String) = l + r
  }

  class handler[F[_]](implicit val ops: SampleOps[F], val str: SampleString[F])

  def concat =
    group("str") |> get |>
      operation("concat") |>
      queryParam[String]("left").as("l") |>
      queryParam[String]("right").as("r") |>
      plain[String]

  def combine = get |> operation("combine") |> capture[Int]("y") |> $$[DebugParams[Combine]]

  def sum = operation("sum") |> capture[Int]("y") |> get |> $$[Int]

  def stats = operation("stats") |> reqBody[Seq[BigDecimal]] |> post |> $$[StatsRes]

  def statsq = operation("statsq") |> queryParams[BigDecimal]("num") |> get |> $$[StatsRes]

  def intops = queryParam[Client]("x") |> (combine ~ sum)

  def dist = operation("sqrtMean") |> formField[Double]("a") |> formField[Double]("b") |> post[Double]

  def sample = group("ops") |> (intops <> stats <> statsq <> dist)

  def api = tagPrefix("test") |> (concat <> sample)
}
