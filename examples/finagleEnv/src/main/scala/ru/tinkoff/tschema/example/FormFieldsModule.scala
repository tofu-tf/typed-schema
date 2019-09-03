package ru.tinkoff.tschema
package example

import cats.Monad
import org.manatki.derevo.derive
import org.manatki.derevo.tethysInstances.{tethysReader, tethysWriter}
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.finagle.{MkService, RoutedPlus}
import ru.tinkoff.tschema.swagger._
import syntax._
import finagle.tethysInstances._

class FormFieldsModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import FormFieldsModule._

  val route = MkService[H](api)(handler)
  val swag = MkSwagger(api)
}

object FormFieldsModule {
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
}
