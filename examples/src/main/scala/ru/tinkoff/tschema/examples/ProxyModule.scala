package ru.tinkoff.tschema.examples

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.Credentials
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport.marshaller
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances._
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.akkaHttp.auth.{BasicAuthenticator, BearerAuthenticator}
import ru.tinkoff.tschema.swagger.{SwaggerBuilder, _}
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL.QueryParam
import shapeless.{HNil, Witness}

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
}
