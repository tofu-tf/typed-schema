package ru.tinkoff.tschema

package object swagger {
  type SwaggerDescription = String

  type StatusCode = Int
  type MediaType  = String

  type Swagger[A] = SwaggerTypeable[A]

  implicit class SwaggerOps[x](x: => x) {
    def mkSwagger(implicit swagger: MkSwagger[x]): MkSwagger[x]            = swagger
    def swaggerMapper(implicit mapper: SwaggerMapper[x]): SwaggerMapper[x] = mapper
  }

  val MagnoliaSwagger: Swagger.type = Swagger

  @deprecated("use tschema.swagger.MkSwagger", since = "0.12.1")
  val MkSwagger: tschema.swagger.MkSwagger.type = tschema.swagger.MkSwagger
  type MkSwagger[T] = tschema.swagger.MkSwagger[T]

  type SwaggerBuilder = tschema.swagger.SwaggerBuilder
  val SwaggerBuilder: tschema.swagger.SwaggerBuilder.type = tschema.swagger.SwaggerBuilder
}
