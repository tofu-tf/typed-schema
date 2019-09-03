package ru.tinkoff.tschema.example

import cats.Monad
import ru.tinkoff.tschema.finagle.{MkService, RoutedPlus}
import ru.tinkoff.tschema.swagger.MkSwagger
import ru.tinkoff.tschema.finagle.tethysInstances._
import cats.instances.string._
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.tethysWriter
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.syntax._

class ProxyModule[H[_] : Monad: RoutedPlus] extends ExampleModule[H] {
  import ProxyModule._
  val swag  = MkSwagger(api)
  val route = MkService[H](api)(handler)
}

object ProxyModule{
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

}
