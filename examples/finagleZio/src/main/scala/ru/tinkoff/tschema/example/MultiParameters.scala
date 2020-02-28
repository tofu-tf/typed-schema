package ru.tinkoff.tschema
package example

import derevo.cats.show
import derevo.derive
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.param.HttpParam
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, SwaggerBuilder, _}
import syntax._
import ru.tinkoff.tschema.finagle.showInstances._
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._

object MultiParameters extends ExampleModule {
  final case class Child(childName: String, childAge: Int)

  object Child {
    implicit val params: HttpParam[Child]         = HttpParam.generate
    implicit val swagger: AsOpenApiParam[Child]   = AsOpenApiParam.generate
    implicit val typeable: SwaggerTypeable[Child] = MagnoliaSwagger.derive
  }

  @derive(Swagger, HttpParam, AsOpenApiParam, show)
  final case class User(name: String, age: Int, child: Child)

  @derive(HttpParam, AsOpenApiParam, show, Swagger)
  final case class Page(from: Int, count: Int, opt: Option[String])

  def route                = MkService[Http](api)(handler)
  def swag: SwaggerBuilder = MkSwagger(api)

  def api =
    tagPrefix('multi) |> ((
      operation('describe) |> get |> queryParam[User]('user) |> $$[User]
    ) <|> (
      operation('pageDescr) |> get |>
        queryParam[Option[Page]]('page) |> $$[Option[Page]]
    ))

  object handler {
    def describe(user: User)          = user
    def pageDescr(page: Option[Page]) = page
  }

}
