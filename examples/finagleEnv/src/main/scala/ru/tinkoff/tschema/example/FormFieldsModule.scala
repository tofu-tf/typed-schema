package ru.tinkoff.tschema.example

import cats.Monad
import cats.syntax.either._
import derevo.derive
import derevo.tethys.{tethysReader, tethysWriter}
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.finagle.{MkService, RoutedPlus}
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.param.{HttpSingleParamReq, ParseParamError}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import tethys._
import tethys.jackson._

class FormFieldsModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import FormFieldsModule._

  val route = MkService[H](api)(handler)
  val swag = MkSwagger(api)
}

object FormFieldsModule {
  @derive(tethysWriter, tethysReader, Swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag("formFields") |>
    urlencoded <|> multipart <|> encoded

  def urlencoded =
    operation("person") |>
    formField[String]("name") |>
    formField[Long]("age") |>
    put |> json[Person]

  def multipart =
    operation("person") |>
    multipartField[String]("name") |>
    multipartField[Long]("age") |>
    post |> json[Person]
  

  // json-encoded form param example
  implicit val personParam: HttpSingleParamReq[Person] =
    (input) => input.toString.jsonAs[Person].leftMap(e => ParseParamError(e.getMessage))

  def encoded =
    operation("changeAge") |>
    multipartField[Person]("person") |>
    multipartField[Long]("age") |>
    post |> json[Person]

  object handler {
    def person(name: String, age: Long): Person = Person(name, age)
    def changeAge(person: Person, age: Long): Person = person.copy(age = age)
  }
}
