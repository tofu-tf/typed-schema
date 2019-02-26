package ru.tinkoff.tschema.scalaz.example
import ru.tinkoff.tschema.akkaHttp.{HttpParam, Param}
import ru.tinkoff.tschema.akkaHttp.ParamSource.All
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, MagnoliaSwagger, SwaggerTypeable}
import scalaz.deriving

@deriving(SwaggerTypeable, AsOpenApiParam, HttpParam)
final case class Foo(name: String, age: Long)

final case class Bar(name: String, age: Long) {
  val swagger: SwaggerTypeable[Bar]  = MagnoliaSwagger.derive
  val asOpenApi: AsOpenApiParam[Bar] = AsOpenApiParam.generate
  val para: Param.PAll[Bar]          = HttpParam.generate
}
