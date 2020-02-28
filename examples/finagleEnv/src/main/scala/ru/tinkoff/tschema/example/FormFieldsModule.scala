package ru.tinkoff.tschema
package example

import cats.Monad
import derevo.derive
import derevo.tethys.{tethysReader, tethysWriter}
import ru.tinkoff.tschema.finagle.{MkService, RoutedPlus}
import ru.tinkoff.tschema.swagger._
import syntax._
import finagle.tethysInstances._
import ru.tinkoff.tschema.custom.syntax._

class FormFieldsModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import FormFieldsModule._

  val route = MkService[H](api)(handler)
  val swag = MkSwagger(api)
}

object FormFieldsModule {
  @derive(tethysWriter, tethysReader, Swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag('formFields) |>
      operation('person) |>
      formField[String]('name) |>
      formField[Long]('age) |>
      put |> json[Person]

  object handler {
    def person(name: String, age: Long): Person = Person(name, age)
  }
}
