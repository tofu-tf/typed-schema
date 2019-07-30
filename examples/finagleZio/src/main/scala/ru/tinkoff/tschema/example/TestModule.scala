package ru.tinkoff.tschema.example

import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances._
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.param.{Param, ParamSource}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import zio.ZIO

object definitions {

  @derive(tethysReader, tethysWriter, swagger)
  case class StatsRes(theMean: BigDecimal, disperse: BigDecimal, median: BigDecimal)

  @derive(tethysReader, tethysWriter, swagger)
  case class Combine(source: CombSource, res: CombRes)

  @derive(tethysReader, tethysWriter, swagger)
  case class CombSource(x: Int, y: Int)

  @derive(tethysReader, tethysWriter, swagger)
  case class CombRes(mul: Int, sum: Int)

  case class Client(value: Int)

  def concat =
    operation('concat) |>
      queryParam[String]('left).as('l) |>
      queryParam[String]('right).as('r) |>
      get |> complete[String]

  def combine = get |> operation('combine) |> capture[Int]('y) |> $$[DebugParams[Combine]]

  def sum = operation('sum) |> capture[Int]('y) |> get |> $$[Int]

  def stats = operation('stats) |> reqBody[Seq[BigDecimal]] |> post |> $$[StatsRes]

  def statsq = operation('statsq) |> queryParams[BigDecimal]('num) |> get |> $$[StatsRes]

  def intops = queryParam[Client]('x) |> (combine ~ sum)

  def dist = operation('sqrtMean) |> formField[Double]('a) |> formField[Double]('b) |> post[Double]

  def api = tagPrefix('test) |> (concat <> intops <> stats <> statsq <> dist)
}

object TestModule extends ExampleModule {

  import definitions._

  implicit lazy val clientFromParam: Param[ParamSource.All, Client] =
    Param.intParam.map(Client)
  implicit val clientSwagger: SwaggerTypeable[Client] =
    SwaggerTypeable.swaggerTypeableInteger.as[Client]

  trait Mutate {
    def mutate(value: Long)          = java.lang.Long.toBinaryString(value)
    def concat(l: String, r: String) = l + r
  }

  object handler extends Mutate {

    def combine(x: Client, y: Int) =
      Combine(
        CombSource(x.value, y),
        CombRes(mul = x.value * y, sum = x.value + y)
      )

    def sum(x: Client, y: Int): Example[Int] = ZIO.succeed(x.value + y)

    def sqrtMean(a: Double, b: Double): Double = Math.sqrt((a * a + b * b) / 2)

    def stats(body: Seq[BigDecimal]) = {
      val mean = body.sum / body.size
      val mid  = body.size / 2
      val median =
        if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std = body.view.map(x => x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }

    def statsq(num: Seq[BigDecimal]) = stats(num)
  }

  val swag = MkSwagger(api)

  val route = MkService[Http](api)(handler)
}
