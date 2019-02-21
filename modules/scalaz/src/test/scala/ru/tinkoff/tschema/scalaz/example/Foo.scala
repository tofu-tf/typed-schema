package ru.tinkoff.tschema.scalaz.example
import ru.tinkoff.tschema.akkaHttp.Param
import ru.tinkoff.tschema.swagger.{AsOpenApiParam, SwaggerTypeable}
import scalaz.deriving

@deriving(SwaggerTypeable, AsOpenApiParam)
final case class Foo(name: String, age: Long)
