package ru.tinkoff.tschema.example

import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.MkSwagger
import ru.tinkoff.tschema.finagle.tethysInstances._
import cats.instances.string._
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances.swagger

object ProxyModule extends ExampleModule {

  import ru.tinkoff.tschema.syntax._

  @derive(tethysWriter, swagger)
  final case class ProxyEcho(
      foo: String,
      bar: String,
      all: Map[String, String]
  )

  def api =
    get |>
      operation('proxy) |>
      queryParam[String]('foo) |>
      queryParam[String]('bar) |>
      allQuery('rest) |>
      $$[ProxyEcho]

  object handler {
    def proxy(foo: String, bar: String, rest: Map[String, String]) = ProxyEcho(foo, bar, rest)
  }

  val swag  = MkSwagger(api)
  val route = MkService[Http](api)(handler)
}
