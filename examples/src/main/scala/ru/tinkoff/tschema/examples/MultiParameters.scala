package ru.tinkoff.tschema.examples
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Param, ParamSource}
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, SwaggerBuilder}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.syntax._

object MultiParameters extends ExampleModule {
  final case class Child(childName: String, childAge: Int)

  object Child{
    final implicit val params: Param.Typeclass[Child] = Param.generate
    final implicit val swagger: AsOpenApiParam[Child] = AsOpenApiParam.generate
  }

  final case class User(name: String, age: Int, child: Child)

  object User {
    final implicit val params: Param[ParamSource.All, User] =
      Param.generate[User]
    final implicit val swaggerParam: AsOpenApiParam[User] =
      AsOpenApiParam.generate
  }

  def route: Route = MkRoute(api)(handler)
  def swag: SwaggerBuilder = MkSwagger(api)(())

  def api =
    tagPrefix('multi) |> operation('describe) |>
      get |> queryParam[User]('user) |> $$[String]

  object handler {
    def describe(user: User) = user.toString
  }

}
