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
}
