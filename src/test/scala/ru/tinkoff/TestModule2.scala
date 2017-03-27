package ru.tinkoff

import java.util.ResourceBundle

import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import io.circe.generic.JsonCodec
import ru.tinkoff.tschema.FromQueryParam
import ru.tinkoff.tschema.akkaHttp._
import ru.tinkoff.tschema.limits._
import ru.tinkoff.tschema.macros.NamedImpl
import ru.tinkoff.tschema.swagger.SwaggerTypeable._
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
object TestModule2 {

  @JsonCodec case class StatsRes(mean: BigDecimal, disperse: BigDecimal, median: BigDecimal)
  @JsonCodec case class Combine(source: CombSource, res: CombRes)
  @JsonCodec case class CombSource(x: Int, y: Int)
  @JsonCodec case class CombRes(mul: Int, sum: Int)

  case class Client(value: Int)

  implicit lazy val statResTypeable = genNamedTypeable[StatsRes]("Stats")
  implicit lazy val combSourceTypeable = genNamedTypeable[CombSource]("CombSource")
  implicit lazy val combResTypeable = genNamedTypeable[CombRes]("CombRes")
  implicit lazy val combineTypeable = genNamedTypeable[Combine]("Combine")

  implicit lazy val clientFromParam = FromQueryParam.intParam.map(Client)
  implicit lazy val clientSwaggerParam = AsSwaggerParam[Client](SwaggerIntValue())

  implicit lazy val bundle = ResourceBundle.getBundle("swagger")

  def combine = keyPrefix('combine) :> capture[Int]('y) :> Get[Combine]

  def sum = keyPrefix('sum) :> capture[Int]('y) :> (limit(1) / minute ! 'x) :> Get[Int]

  def stats = keyPrefix('stats) :> ReqBody[Vector[BigDecimal]] :> Post[StatsRes]

  def intops = queryParam[Client]('x) :> (combine <|> sum)

  def api1 = keyPrefix('combine) {
    queryParam[Client]('x) {
      capture[Int]('y) {
        Get[Combine]
      }
    }
  }
  /*    tagPrefix('test) {*/
  def api2 =
    keyPrefix('stats) {
      ReqBody[Vector[BigDecimal]] {
        Post[StatsRes]
      }
    }
  /*}*/

  object handler {
    def concat(left: String, right: String) = left + right

    def combine(x: Client, y: Int) = Combine(CombSource(x.value, y), CombRes(mul = x.value * y, sum = x.value + y))

    def sum(x: Client, y: Int): Future[Int] = Future(x.value + y)

    def stats(body: Vector[BigDecimal]) = {
      val mean = body.sum / body.size
      val mid = body.size / 2
      val median = if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std = body.view.map(x ⇒ x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }
  }

  implicit val limitHandler = LimitHandler.trieMap

  val pp = keyPrefix('test) :> queryParam[String]('left) :> queryParam[String]('right) :> (limit(1) / hour ! 'left) :> Get[String]

  pp.serve

//  api.mkSwagger

//  val srv = api1.serve
//  api2.serve
//  (api1 <|> api2).serve

//  val impl = NamedImpl[handler.type, srv.Input]

//  def main(args: Array[String]): Unit = {
//    println(impl.description)
//    println(srv)
//    "asdsad".split('a')
//  }

//  lazy val route: Route = api.route(handler)
}

