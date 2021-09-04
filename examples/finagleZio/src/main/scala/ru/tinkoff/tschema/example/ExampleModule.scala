package ru.tinkoff.tschema.example

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.swagger.SwaggerBuilder

trait ExampleModule {
  def route: Http[Response]
  def swag: SwaggerBuilder
}
