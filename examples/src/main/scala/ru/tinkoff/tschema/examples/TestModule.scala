package ru.tinkoff.tschema.examples

import java.util.{Locale, ResourceBundle}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.FromQueryParam
import ru.tinkoff.tschema.akkaHttp.MkRoute
import ru.tinkoff.tschema.swagger.SwaggerTypeable._
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import akka.http.scaladsl.server.Directives._enhanceRouteWithConcatenation
import io.circe.generic.JsonCodec
import ru.tinkoff.tschema.utils.json.circeCodec

import scala.concurrent.Future
import scala.concurrent.duration._

object definitions {

  @SwaggerTyping(name = "PEKA")
  @circeCodec(derivation = true, snake = true)
  case class StatsRes(theMean: BigDecimal, disperse: BigDecimal, median: BigDecimal)

  @SwaggerTyping(named = false)
  @JsonCodec
  case class Combine(source: CombSource, res: CombRes)

  @SwaggerTyping
  @JsonCodec
  case class CombSource(x: Int, y: Int)

  @SwaggerTyping
  @JsonCodec
  case class CombRes(mul: Int, sum: Int)

  case class Client(value: Int)

  def concat = operation('concat) |> queryParam[String]('left).as('l) |> queryParam[String]('right).as('r) |> get[String]

  def combine = operation('combine) |> capture[Int]('y)  |> get[Combine]

  def sum = operation('sum) |> capture[Int]('y) |> get[Int]

  def stats = operation('stats) |> reqBody[Seq[BigDecimal]] |> post[StatsRes]

  def intops = queryParam[Client]('x) |> (combine ~ sum)

  def api = tagPrefix('test) |> (concat <> intops <> stats)

  def api2 = tagPrefix('test2) {
    operation('concat) {
      queryParam[String]('left).as('l) {
        queryParam[String]('right).as('r) {
          get[String]
        }
      }
    } ~
    queryParam[Client]('x) {
      operation('combine) {
        capture[Int]('y) {
            get[Combine]
        }
      } ~
      operation('sum) {
        capture[Int]('y) {
          get[Int]
        }
      }
    } ~
    operation('stats) {
      reqBody[Seq[BigDecimal]] {
        post[StatsRes]
      }
    }
  }
}

object TestModule extends ExampleModule {
  implicit val system  = ActorSystem("swagger-test")
  implicit val mat = ActorMaterializer()

  import definitions._

  implicit lazy val clientFromParam = FromQueryParam.intParam.map(Client)
  implicit val clientSwagger: SwaggerTypeable[Client] = SwaggerTypeable.swaggerTypeableInteger.as[Client]

  import scala.concurrent.ExecutionContext.Implicits.global

  trait Mutate{
    def mutate(value: Long) = java.lang.Long.toBinaryString(value)
    def concat(l: String, r: String) = l + r
  }

  object handler extends Mutate {


    def combine(x: Client, y: Int) = Combine(CombSource(x.value, y), CombRes(mul = x.value * y, sum = x.value + y))

    def sum(x: Client, y: Int): Future[Int] = Future(x.value + y)

    def stats(body: Seq[BigDecimal]) = {
      val mean = body.sum / body.size
      val mid = body.size / 2
      val median = if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std = body.view.map(x => x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }
  }


  val descriptions = PathDescription.i18n(ResourceBundle.getBundle("swagger", Locale.forLanguageTag("ru")))
  val swagger1 = api.mkSwagger.describe(descriptions)
  val swagger2 = api2.mkSwagger.describe(descriptions)

  val route1 = MkRoute(api)(handler)
  val route2 = MkRoute(api2)(handler)

  val swagger = swagger1 ++ swagger2
  val route = route1 ~ route2
}

