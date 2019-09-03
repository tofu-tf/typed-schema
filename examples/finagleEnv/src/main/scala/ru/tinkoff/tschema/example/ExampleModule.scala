package ru.tinkoff.tschema.example

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.swagger.SwaggerBuilder

trait ExampleModule[H[_]] {
  def route: H[Response]
  def swag: SwaggerBuilder
}


