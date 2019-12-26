package tschema.scalaz.example
import tschema.param.{HttpParam, Param}
import tschema.swagger.{AsOpenApiParam, MagnoliaSwagger, SwaggerTypeable}
import scalaz.deriving

@deriving(SwaggerTypeable, AsOpenApiParam, HttpParam)
final case class Foo(name: String, age: Long)

final case class Bar(name: String, age: Long) {
  val swagger: SwaggerTypeable[Bar]  = MagnoliaSwagger.derive
  val asOpenApi: AsOpenApiParam[Bar] = AsOpenApiParam.generate
  val para: Param.PAll[Bar]          = HttpParam.generate
}
