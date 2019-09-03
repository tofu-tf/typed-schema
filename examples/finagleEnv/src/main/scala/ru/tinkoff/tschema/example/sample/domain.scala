package ru.tinkoff.tschema.example.sample
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances._
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.param.{Param, ParamSource}
import ru.tinkoff.tschema.swagger.SwaggerTypeable

@derive(tethysReader, tethysWriter, swagger)
case class StatsRes(theMean: BigDecimal, disperse: BigDecimal, median: BigDecimal)

@derive(tethysReader, tethysWriter, swagger)
case class Combine(source: CombSource, res: CombRes)

@derive(tethysReader, tethysWriter, swagger)
case class CombSource(x: Int, y: Int)

@derive(tethysReader, tethysWriter, swagger)
case class CombRes(mul: Int, sum: Int)

case class Client(value: Int)

object Client {
  implicit lazy val clientFromParam: Param[ParamSource.All, Client] =
    Param.intParam.map(Client(_))
  implicit val clientSwagger: SwaggerTypeable[Client] =
    SwaggerTypeable.swaggerTypeableInteger.as[Client]
}
