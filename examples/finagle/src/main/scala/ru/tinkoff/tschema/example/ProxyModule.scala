package ru.tinkoff.tschema.example

import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.MkSwagger
import ru.tinkoff.tschema.finagle.showInstances._
import cats.instances.string._

object ProxyModule extends ExampleModule {

  import ru.tinkoff.tschema.syntax._

  def api =
    get |>
      operation('proxy) |>
      queryParam[String]('foo) |>
      queryParam[String]('bar) |>
      allQuery('rest) |>
      $$[String]

  object handler {
    def proxy(foo: String, bar: String, rest: Map[String, String]): String =
      s"Proxy echo: $foo, $bar, $rest"
  }

  val swag  = MkSwagger(api)
  val route = MkService[Http](api)(handler)
}
