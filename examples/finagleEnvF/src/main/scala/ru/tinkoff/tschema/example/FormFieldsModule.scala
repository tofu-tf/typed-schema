package ru.tinkoff.tschema.example

import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.{tethysReader, tethysWriter}
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger._
import syntax._
import finagle.tethysInstances._


object FormFieldsModule extends ExampleModule {

  @derive(tethysWriter, tethysReader, swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag('formFields) |>
      operation('person) |>
      formField[String]('name) |>
      formField[Long]('age) |>
      put[Person]

  object handler {
    def person(name: String, age: Long): Person = Person(name, age)
  }

  val route = MkService[Http](api)(handler)
  val swag  = MkSwagger(api)
}
