package ru.tinkoff.tschema.swagger

object syntax {

  implicit class SwaggerOps[x](x: ⇒ x) {
    def mkSwagger(implicit derive: DerivedMkSwagger[x]) = derive.mkSwagger
  }
}
