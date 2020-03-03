package ru.tinkoff.tschema.example

import cats.Monad
import cats.instances.string._
import derevo.derive
import derevo.tethys.tethysWriter
import ru.tinkoff.tschema.finagle.RoutedPlus
import ru.tinkoff.tschema.finagle.tethysInstances._
import tschema.swagger._
import tschema.custom.syntax._
import tschema.finagle.MkService
import tschema.syntax._

class ProxyModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import ProxyModule._
  val swag = MkSwagger(api)
  val route = MkService[H](api)(handler)
}

object ProxyModule {
  @derive(tethysWriter, Swagger)
  final case class ProxyEcho(
      foo: String,
      bar: String,
      all: Map[String, String]
  )

  def api =
    get |>
      operation("proxy") |>
      queryParam[String]("foo") |>
      queryParam[String]("bar") |>
      allQuery("rest") |>
      json[ProxyEcho]

  object handler {
    def proxy(foo: String, bar: String, rest: Map[String, String]) = ProxyEcho(foo, bar, rest)
  }

}
