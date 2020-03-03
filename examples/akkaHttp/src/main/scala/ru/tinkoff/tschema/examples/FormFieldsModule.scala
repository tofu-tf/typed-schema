package ru.tinkoff.tschema
package examples

import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import derevo.circe.{decoder, encoder}
import derevo.derive
import ru.tinkoff.tschema.akkaHttp.MkRoute
import tschema.swagger._
import tschema.syntax._

object FormFieldsModule extends ExampleModule {
  @derive(encoder, decoder, Swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag("formFields") |>
      operation("person") |>
      formField[String]("name") |>
      formField[Long]("age") |>
      put |> $$[Person]

  object handler {
    def person(name: String, age: Long): Person = Person(name, age)
  }

  val route   = MkRoute(api)(handler)
  val swag = MkSwagger(api)
}
