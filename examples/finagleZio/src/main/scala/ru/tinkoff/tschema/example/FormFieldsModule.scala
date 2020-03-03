package ru.tinkoff.tschema
package example

import derevo.derive
import derevo.tethys.{tethysReader, tethysWriter}
import tschema.finagle.MkService
import tschema.swagger._
import tschema.syntax._
import finagle.tethysInstances._


object FormFieldsModule extends ExampleModule {

  @derive(tethysWriter, tethysReader, Swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag("formFields") |>
      operation("person") |>
      formField[String]("name") |>
      formField[Long]("age") |>
      put[Person]

  object handler {
    def person(name: String, age: Long): Person = Person(name, age)
  }

  val route = MkService[Http](api)(handler)
  val swag  = MkSwagger(api)
}
