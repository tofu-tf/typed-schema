package ru.tinkoff.tschema.examples

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.akkaHttp.MkRoute
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._

object FormFieldsModule extends ExampleModule {
  @derive(encoder, decoder, swagger)
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

  val route   = MkRoute(api)(handler)
  val swag = MkSwagger(api)(())
}
