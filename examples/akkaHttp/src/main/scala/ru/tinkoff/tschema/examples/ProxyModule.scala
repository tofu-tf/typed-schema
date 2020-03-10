package ru.tinkoff.tschema.examples
import ru.tinkoff.tschema.akkaHttp.MkRoute
import ru.tinkoff.tschema.swagger.MkSwagger

object ProxyModule extends ExampleModule {

  import ru.tinkoff.tschema.syntax._

  def api =
    get |>
      operation("proxy") |>
      queryParam[String]("foo") |>
      queryParam[String]("bar") |>
      allQuery("rest") |>
      $$[String]


  object handler {
    def proxy(foo: String, bar: String, rest: Map[String, String]): String =
      s"Proxy echo: $foo, $bar, $rest"
  }

  val swag = MkSwagger(api)
  val route = MkRoute(api)(handler)
}
