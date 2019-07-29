package ru.tinkoff.tschema.example
import scala.io.Source

object Lol extends App{
  println(Option(getClass.getResourceAsStream("/META-INF/resources/webjars/swagger-ui-dist/3.23.1/swagger-ui-bundle.js")))

//  println(Source.fromResource("META-INF/resources/webjars/swagger-ui-dist/3.23.1/swagger-ui-bundle.js").getLines().mkString("\n"))
}
