package ru.tinkoff

import java.util.ResourceBundle
import java.util.ResourceBundle

import akka.http.scaladsl.server._
import io.circe.generic.JsonCodec
import ru.tinkoff.tschema.serve.FromQueryParam
import ru.tinkoff.tschema.swagger.{AsSwaggerParam, StaticDescription, SwaggerIntValue, SwaggerTag}
import ru.tinkoff.tschema.swagger.SwaggerTypeable._
import ru.tinkoff.tschema.typeDSL._
import ru.tinkoff.tschema.syntax._

object TestModule {

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

  implicit val bundle = ResourceBundle.getBundle("swagger")

  def concat = prefix("concat") :> queryParam[String]('left) :> queryParam[String]('right) :> Get[String]

  def combine = prefix("combine") :> queryParam[Client]('x) :> capture[Int]('y) :> Get[Combine]

  def stats = prefix("stats") :> ReqBody[Vector[BigDecimal]] :> Post[StatsRes]

  def api = tagPrefix('test) :> (concat <|> combine <|> stats)
  object handler {
    def concat(x: String, y: String) = x + y

    def combine(x: Client, y: Int) = Combine(CombSource(x.value, y), CombRes(mul = x.value * y, sum = x.value + y))

    def stats(xs: Vector[BigDecimal]) = {
      val mean = xs.sum / xs.size
      val mid = xs.size / 2
      val median = if (xs.size % 2 == 1) xs(mid) else (xs(mid) + xs(mid - 1)) / 2
      val std = xs.view.map(x ⇒ x * x).sum / xs.size - mean * mean
      StatsRes(mean, std, median)
    }
  }
//  lazy val route: Route = api.route(handler)

  val mkSwagger = api.mkSwagger

  def tagInfo: Vector[SwaggerTag] = Vector(
    SwaggerTag("test", Some(StaticDescription("Test modules")))
  )
}

