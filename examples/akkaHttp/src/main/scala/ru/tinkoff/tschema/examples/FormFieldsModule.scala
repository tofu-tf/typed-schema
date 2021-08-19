package ru.tinkoff.tschema.examples

import cats.syntax.either._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import derevo.circe.{decoder, encoder}
import derevo.derive
import io.circe.parser._
import ru.tinkoff.tschema.akkaHttp.MkRoute
import ru.tinkoff.tschema.param.{HttpSingleParamReq, ParseParamError}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._

object FormFieldsModule extends ExampleModule {
  @derive(encoder, decoder, Swagger)
  final case class Person(name: String, age: Long)

  def api =
    tag("formFields") |>
      urlencoded <|> multipart <|> encoded

  def urlencoded =
    operation("person") |>
      formField[String]("name") |>
      formField[Long]("age") |>
      put |> $$[Person]

  def multipart =
    operation("person") |>
      multipartField[String]("name") |>
      multipartField[Long]("age") |>
      post |> $$[Person]

  // json-encoded form param example
  implicit val personParam: HttpSingleParamReq[Person] =
    (input) => decode[Person](input.toString).leftMap(e => ParseParamError(e.getMessage))

  def encoded =
    operation("changeAge") |>
      multipartField[Person]("person") |>
      multipartField[Long]("age") |>
      post |> $$[Person]

  object handler {
    def person(name: String, age: Long): Person      = Person(name, age)
    def changeAge(person: Person, age: Long): Person = person.copy(age = age)
  }

  val route = MkRoute(api)(handler)
  val swag  = MkSwagger(api)
}
