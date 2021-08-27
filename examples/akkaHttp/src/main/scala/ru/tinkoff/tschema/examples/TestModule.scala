package ru.tinkoff.tschema
package examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import io.circe.derivation.renaming.snakeCase
import derevo.circe.{decoder, encoder}
import derevo.derive
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.param.{Param, ParamSource}
import ru.tinkoff.tschema.swagger.Swagger
import ru.tinkoff.tschema.swagger.MkSwagger
import syntax._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.swagger.SwaggerTypeable
import ru.tinkoff.tschema.typeDSL.{As, QueryParam}
import shapeless.labelled.FieldType
import shapeless._

import scala.concurrent.Future

object definitions {

  @derive(encoder(snakeCase, None), decoder(snakeCase, false, None), Swagger)
  case class StatsRes(theMean: BigDecimal, disperse: BigDecimal, median: BigDecimal)

  @derive(encoder, decoder, Swagger)
  case class Combine(source: CombSource, res: CombRes)
  @derive(encoder, decoder, Swagger)
  case class CombSource(x: Int, y: Int)

  @derive(encoder, decoder, Swagger)
  case class CombRes(mul: Int, sum: Int)

  case class Client(value: Int)

  def concat =
    operation("concat") |> queryParam[String]("left")
      .as("l") |> queryParam[String]("right").as("r") |> get |> complete[String]

  def combine =
    get |> operation("combine") |> capture[Int]("y") |> $$[DebugParams[Combine]]

  def sum = get |> operation("sum") |> capture[Int]("y") |> $$[Int]

  def stats =
    post |> operation("stats") |> reqBody[Seq[BigDecimal]] |> $$[StatsRes]

  def statsq =
    get |> operation("statsq") |> queryParams[BigDecimal]("num") |> $$[StatsRes]

  def intops = queryParam[Client]("x") |> (combine ~ sum)

  def dist =
    operation("sqrtMean") |> formField[Double]("a") |> formField[Double]("b") |> post[
      Double
    ]

  def api = tagPrefix("test") |> (concat <> intops <> stats <> statsq <> dist)
}

object TestModule extends ExampleModule {
  implicit val system = ActorSystem("swagger-test")

  import definitions._

  implicit lazy val clientFromParam: Param[ParamSource.All, Client] =
    Param.intParam.map(Client)
  implicit val clientSwagger: Swagger[Client]                       = Swagger[Int].as[Client]

  import scala.concurrent.ExecutionContext.Implicits.global

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

    def sum(x: Client, y: Int): Future[Int] = Future(x.value + y)

    def sqrtMean(a: Double, b: Double): Double = Math.sqrt((a * a + b * b) / 2)

    def stats(body: Seq[BigDecimal]) = {
      val mean   = body.sum / body.size
      val mid    = body.size / 2
      val median =
        if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std    = body.view.map(x => x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }

    def statsq(num: Seq[BigDecimal]) = stats(num)
  }

  implicitly[SwaggerTypeable[Seq[BigDecimal]]]

  val swag = MkSwagger(api)

  val route = MkRoute(api)(handler)
}
