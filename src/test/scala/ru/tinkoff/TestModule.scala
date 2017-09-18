package ru.tinkoff

import java.util.ResourceBundle

import io.circe.generic.JsonCodec
import ru.tinkoff.tschema.FromQueryParam
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Routable, Serve}
import ru.tinkoff.tschema.limits
import limits._
import ru.tinkoff.tschema.swagger.SwaggerTypeable._
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HNil, Witness => W}
import shapeless.record.Record
import ru.tinkoff.tschema.swagger._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.Printer
import ru.tinkoff.tschema.limits.LimitHandler.LimitRate
import shapeless.labelled.FieldType
import io.circe.syntax._

import scala.concurrent.duration._

object definitions {
  @JsonCodec case class StatsRes(mean: BigDecimal, disperse: BigDecimal, median: BigDecimal)
  @JsonCodec case class Combine(source: CombSource, res: CombRes)
  @JsonCodec case class CombSource(x: Int, y: Int)
  @JsonCodec case class CombRes(mul: Int, sum: Int)

  case class Client(value: Int)

  def concat = operation('concat) |> queryParam[String]('left) |> queryParam[String]('right) |> get[String]

  def combine = operation('combine) |> capture[Int]('y) |> (limit ! 'x) |> get[Combine]

  def sum = operation('sum) |> capture[Int]('y) |> get[Int]

  def stats = operation('stats) |> reqBody[Vector[BigDecimal]] |> post[StatsRes]

  def intops = queryParam[Client]('x) |> (combine ~ sum)

  def api = tagPrefix('test) |> (concat <> intops <> stats)

  def api2 = tagPrefix('test) {
    operation('concat) {
      queryParam[String]('left) {
        queryParam[String]('right) {
          get[String]
        }
      }
    } ~
      queryParam[Client]('x) {
        operation('combine) {
          capture[Int]('y) {
            (limit ! 'x) {
              get[Combine]
            }
          }
        } ~
          operation('sum) {
            capture[Int]('y) {
              get[Int]
            }
          }
      } ~
      operation('stats) {
        reqBody[Vector[BigDecimal]] {
          post[StatsRes]
        }
      }
  }
}

object TestModule {

  import definitions._

  implicit lazy val statResTypeable = genNamedTypeable[StatsRes]("Stats")
  implicit lazy val combSourceTypeable = genNamedTypeable[CombSource]("CombSource")
  implicit lazy val combResTypeable = genNamedTypeable[CombRes]("CombRes")
  implicit lazy val combineTypeable = genNamedTypeable[Combine]("Combine")

  implicit lazy val clientFromParam = FromQueryParam.intParam.map(Client)
  implicit lazy val clientSwaggerParam = AsSwaggerParam[Client](SwaggerIntValue())

  implicit lazy val bundle = ResourceBundle.getBundle("swagger")

  import scala.concurrent.ExecutionContext.Implicits.global

  object handler {
    def concat(left: String, right: String) = left + right

    def combine(x: Client, y: Int) = Combine(CombSource(x.value, y), CombRes(mul = x.value * y, sum = x.value + y))

    def sum(x: Client, y: Int): Future[Int] = Future(x.value + y)

    def stats(body: Vector[BigDecimal]) = {
      val mean = body.sum / body.size
      val mid = body.size / 2
      val median = if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std = body.view.map(x => x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }

    def mutate(value: Long) = java.lang.Long.toBinaryString(value)
  }

  implicit val limitHandler = LimitHandler.trieMap(_ => LimitRate(1, 1 second))

  val swagger = api.mkSwagger
  val swagger2 = api2.mkSwagger

  val route = MkRoute(api)(handler)
  val route2 = MkRoute(api2)(handler)
  val printer = Printer.spaces2.copy(dropNullValues = true)
  def main(args: Array[String]): Unit = {
    swagger
    .make(SwaggerInfo(title = "test"))
    .paths.foreach{case (name, map) =>
      val (meth, op) = map.head
      println(s"$name : $meth \n------------  ${op.asJson.pretty(printer)}\n------------")}
  }
}

