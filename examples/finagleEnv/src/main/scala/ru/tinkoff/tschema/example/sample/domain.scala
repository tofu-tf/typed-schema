package ru.tinkoff.tschema.example.sample
import derevo.derive
import derevo.tethys._
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.param.{Param, ParamSource}
import ru.tinkoff.tschema.swagger.Swagger
@derive(tethysReader, tethysWriter, Swagger)
case class StatsRes(theMean: BigDecimal, disperse: BigDecimal, median: BigDecimal)

@derive(tethysReader, tethysWriter, Swagger)
case class Combine(source: CombSource, res: CombRes)

@derive(tethysReader, tethysWriter, Swagger)
case class CombSource(x: Int, y: Int)

@derive(tethysReader, tethysWriter, Swagger)
case class CombRes(mul: Int, sum: Int)

case class Client(value: Int)

object Client {
  implicit lazy val clientFromParam: Param[ParamSource.All, Client] =
    Param.intParam.map(Client(_))
  implicit val clientSwagger: Swagger[Client] =
    Swagger[Int].as[Client]
}
