package ru.tinkoff.tschema

package object swagger {
  type SwaggerDescription = String

  type StatusCode = Int
  type MediaType  = String

  implicit class SwaggerOps[x](x: => x) {
    def mkSwagger(implicit swagger: MkSwagger[x]): MkSwagger[x]            = swagger
    def swaggerMapper(implicit mapper: SwaggerMapper[x]): SwaggerMapper[x] = mapper
  }

  val MagnoliaSwagger: tschema.swagger.Swagger.type = tschema.swagger.Swagger

  @deprecated("use tschema.swagger.MkSwagger", since = "0.12.1")
  val MkSwagger: tschema.swagger.MkSwagger.type = tschema.swagger.MkSwagger
  type MkSwagger[T] = tschema.swagger.MkSwagger[T]

  type SwaggerBuilder = tschema.swagger.SwaggerBuilder
  val SwaggerBuilder: tschema.swagger.SwaggerBuilder.type = tschema.swagger.SwaggerBuilder
}
