package ru.tinkoff.tschema.example
package sample

import cats.Monad
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances._
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.finagle.{LiftHttp, MkService, RoutedPlus}
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.param.{Param, ParamSource}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL.{:>, Group}
import tofu.env.Env

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
      operation('concat) |>
      queryParam[String]('left).as('l) |>
      queryParam[String]('right).as('r) |>
      complete[String]

  def combine = get |> operation('combine) |> capture[Int]('y) |> $$[DebugParams[Combine]]

  def sum = operation('sum) |> capture[Int]('y) |> get |> $$[Int]

  def stats = operation('stats) |> reqBody[Seq[BigDecimal]] |> post |> $$[StatsRes]

  def statsq = operation('statsq) |> queryParams[BigDecimal]('num) |> get |> $$[StatsRes]

  def intops = queryParam[Client]('x) |> (combine ~ sum)

  def dist = operation('sqrtMean) |> formField[Double]('a) |> formField[Double]('b) |> post[Double]

  def sample = group("ops") |> (intops <> stats <> statsq <> dist)

  def api = tagPrefix('test) |> (concat <> sample)
}
