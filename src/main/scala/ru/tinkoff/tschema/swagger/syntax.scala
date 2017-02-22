package ru.tinkoff.tschema.swagger

object syntax {

  implicit class SwaggerOps[x](x: ⇒ x) {
    def mkSwagger(implicit swagger: MkSwagger[x]): MkSwagger[x] = swagger
    def swaggerMapper(implicit mapper: SwaggerMapper[x]): SwaggerMapper[x] = mapper
  }
}
