package ru.tinkoff

import java.util.ResourceBundle

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.Route
import io.circe.generic.JsonCodec
import ru.tinkoff.tschema.named.syntax._
import ru.tinkoff.tschema.swagger.syntax._
import ru.tinkoff.tschema.serve.FromQueryParam
import ru.tinkoff.tschema.swagger.SwaggerTypeable._
import ru.tinkoff.tschema.swagger.{AsSwaggerParam, StaticDescription, SwaggerIntValue, SwaggerTag}
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.named
import ru.tinkoff.tschema.typeDSL._
import shapeless.ops.hlist.Reify
import shapeless.ops.record
import shapeless.ops.union
import shapeless.ops.coproduct
import shapeless._
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import ru.tinkoff.tschema.macros.NamedImpl
import ru.tinkoff.tschema.named.{Routable, RoutableResult}
import shapeless.labelled.FieldType

import scala.reflect.runtime.universe._
object TestModule extends App {

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

  def concat = keyPrefix('concat) :> queryParam[String]('left) :> queryParam[String]('right) :> Get[String]

  def combine = keyPrefix('combine) :> capture[Int]('y) :> Get[Combine]

  def sum = keyPrefix('sum) :> capture[Int]('y) :> Get[Int]

  def stats = keyPrefix('stats) :> ReqBody[Vector[BigDecimal]] :> Post[StatsRes]

  def intops = queryParam[Client]('x) :> (combine <|> sum)

  def api = tagPrefix('test) :> (concat <|> intops <|> stats)
  object handler {
    def concat(left: String, right: String) = left + right

    def combine(x: Client, y: Int) = Combine(CombSource(x.value, y), CombRes(mul = x.value * y, sum = x.value + y))

    def sum(x: Client, y: Int) = x.value + y

    def stats(body: Vector[BigDecimal]) = {
      val mean = body.sum / body.size
      val mid = body.size / 2
      val median = if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std = body.view.map(x ⇒ x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }
  }

  val pp = keyPrefix('test) :> queryParam[String]('left) :> queryParam[String]('right) :> Get[String]

  val srv = api.serve

  val impl = NamedImpl[handler.type, srv.Input]

  println(impl.description)

  val routable = the[RoutableResult[impl.Output]]
  coproduct.Align[srv.Output, impl.Output]

  lazy val route: Route = api.route(handler)

  val mkSwagger = api.mkSwagger

  def tagInfo: Vector[SwaggerTag] = Vector(
    SwaggerTag("test", Some(StaticDescription("Test modules")))
  )
}

