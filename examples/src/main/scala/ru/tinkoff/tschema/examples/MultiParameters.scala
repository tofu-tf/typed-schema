package ru.tinkoff.tschema.examples
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.akkaHttp.{HttpParam, MkRoute, Param, ParamSource, Serve}
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, SwaggerBuilder}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._
import scalaz.deriving

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
  final case class Page(from: Int, count: Int)

  @deriving(HttpParam, AsOpenApiParam)
  final case class OptPage(opt: Option[Page])

  def route: Route = MkRoute(api)(handler)
  def swag: SwaggerBuilder = MkSwagger(api)(())

//  implicitly[AsOpenApiParam[Option[Page]]]


  def api =
    tagPrefix('multi) |> ((
      operation('describe) |> get |> queryParam[User]('user) |> $$[String]
    ) <|> (
      operation('pageDescr) |> get |>
        queryParam[OptPage]('page) |> $$[String]
    ))

  object handler {
    def describe(user: User) = user.toString
    def pageDescr(page: OptPage) = page.toString
  }

}
