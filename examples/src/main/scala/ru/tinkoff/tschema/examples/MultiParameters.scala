package ru.tinkoff.tschema.examples
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.param.HttpParam
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, SwaggerBuilder}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._

object MultiParameters extends ExampleModule {
  final case class Child(childName: String, childAge: Int)

  object Child {
    implicit val params: HttpParam[Child] = HttpParam.generate
    implicit val swagger: AsOpenApiParam[Child] = AsOpenApiParam.generate
    implicit val typeable: SwaggerTypeable[Child] = MagnoliaSwagger.derive
  }

  @deriving(SwaggerTypeable, HttpParam, AsOpenApiParam)
  final case class User(name: String, age: Int, child: Child)

  @deriving(HttpParam, AsOpenApiParam)
  final case class Page(from: Int, count: Int, opt: Option[String])


  def route: Route = MkRoute(api)(handler)
  def swag: SwaggerBuilder = MkSwagger(api)(())




  def api =
    tagPrefix('multi) |> ((
      operation('describe) |> get |> queryParam[User]('user) |> $$[String]
    ) <|> (
      operation('pageDescr) |> get |>
        queryParam[Option[Page]]('page) |> $$[String]
    ))

  object handler {
    def describe(user: User) = user.toString
    def pageDescr(page: Option[Page]) = page.toString
  }

}
