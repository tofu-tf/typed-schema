package ru.tinkoff.tschema.swagger.typesafe

import derevo.derive
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, Swagger}
import ru.tinkoff.tschema.syntax._

object MkDescribedSwaggerTest {
  @derive(AsOpenApiParam)
  case class Params1(foo: String, bar: Int)

  @derive(AsOpenApiParam)
  case class Params2(a: String, b: Int, c: Double)

  @derive(AsOpenApiParam)
  case class Params3(lol: Int, kek: String, cheburek: Double)

  @derive(Swagger)
  case class Response(message: String)

  val tag1 =
    tag("tag1") :>
      (
        (
          prefix("method1") :>
            key("meth1") :>
            queryParam[Params1]("params") :>
            $$[Response]
        ) <|>
          (
            prefix("method2") :>
              key("meth2") :>
              queryParam[Params2]("params") :>
              $$[Response]
          )
      )

  val tag2 =
    tag("tag2") :>
      (
        (
          prefix("method3") :>
            key("meth3") :>
            queryParam[Params3]("params") :>
            $$[Response]
        ) <|>
          (
            prefix("method4") :>
              key("meth4") :>
              queryParam[String]("a") :>
              queryParam[String]("b") :>
              $$[Response]
          )
      )

  val api = tag1 <|> tag2

  MkDescribedSwagger(api)
}
