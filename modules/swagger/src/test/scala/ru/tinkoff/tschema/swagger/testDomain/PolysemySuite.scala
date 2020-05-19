package ru.tinkoff.tschema.swagger.testDomain
import org.scalatest.flatspec.AnyFlatSpec
import ru.tinkoff.tschema.swagger.MagnoliaSwagger

class PolysemySuite extends AnyFlatSpec {
  val tolsteeye =
    MagnoliaSwagger.derive[leo.Tolstoi].typ.collectTypes ++
      MagnoliaSwagger.derive[aleksey.Tolstoi].typ.collectTypes

}
