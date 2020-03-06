package ru.tinkoff.tschema
package example

import cats.Monad
import cats.instances.all._
import derevo.cats.show
import derevo.derive
import ru.tinkoff.tschema.custom.{AsResponse, ExceptResult, PlainResult}
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.finagle.{Completing, RoutedPlus}
import ru.tinkoff.tschema.param.HttpParam
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, SwaggerBuilder}
import syntax._
import ru.tinkoff.tschema.swagger.{MkSwagger, Swagger}
import ru.tinkoff.tschema.finagle.MkService

object MultiParams {
  @derive(Swagger, HttpParam, AsOpenApiParam, show)
  final case class User(name: String, age: Int, child: Child)

  @derive(HttpParam, AsOpenApiParam, show, Swagger)
  final case class Page(from: Int, count: Int, opt: Option[String])

  final case class Child(childName: String, childAge: Int)

  object Child {
    implicit val params: HttpParam[Child] = HttpParam.generate
    implicit val swaggerParam: AsOpenApiParam[Child] = AsOpenApiParam.instance
    implicit val swagger: Swagger[Child] = Swagger.instance
  }

  object handler {
    def describe(user:  User) = user
    def pageDescr(page: Option[Page]) = page
  }

  def api =
    tagPrefix("multi") |> ((
      operation("describe") |> get |> queryParam[User]("user") |> plain[User]
    ) <|> (
      operation("pageDescr") |> get |>
        queryParam[Option[Page]]("page") |> plainOpt[Page]
    ))
}

class MultiParameters[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import MultiParams._
  def route = MkService[H](api)(handler)
  def swag: SwaggerBuilder = MkSwagger(api)
}
