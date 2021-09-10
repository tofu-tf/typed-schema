package ru.tinkoff.tschema.example

import cats.syntax.either._
import derevo.derive
import derevo.tethys.{tethysReader, tethysWriter}
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.param.{HttpSingleParamReq, ParseParamError}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import tethys._
import tethys.jackson._

object FormFieldsModule extends ExampleModule {

  @derive(tethysWriter, tethysReader, Swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag("formFields") |>
      urlencoded <|> multipart <|> encoded

  def urlencoded =
    operation("person") |>
      formField[String]("name") |>
      formField[Long]("age") |>
      put[Person]

  def multipart =
    operation("person") |>
      multipartField[String]("name") |>
      multipartField[Long]("age") |>
      post[Person]

  // json-encoded form param example
  implicit val personParam: HttpSingleParamReq[Person] =
    (input) => input.toString.jsonAs[Person].leftMap(e => ParseParamError(e.getMessage))

  def encoded =
    operation("changeAge") |>
      multipartField[Person]("person") |>
      multipartField[Long]("age") |>
      post[Person]

  object handler {
    def person(name: String, age: Long): Person      = Person(name, age)
    def changeAge(person: Person, age: Long): Person = person.copy(age = age)
  }

  val route = MkService[Http](api)(handler)
  val swag  = MkSwagger(api)
}
