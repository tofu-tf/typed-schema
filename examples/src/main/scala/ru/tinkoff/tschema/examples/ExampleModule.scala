package ru.tinkoff.tschema.examples

import akka.http.scaladsl.server.{Directives, Route}
import cats.kernel.Monoid
import ru.tinkoff.tschema.swagger.{MkSwagger, SwaggerBuilder}
import Directives._

trait ExampleModule {
  def route: Route
  def swagger: SwaggerBuilder
}


object ExampleModule {
  implicit val monoidInstance: Monoid[ExampleModule] = new Monoid[ExampleModule] {
    override def empty = new ExampleModule {
      override def route = reject()
      override def swagger = MkSwagger.empty[Nothing]
    }
    override def combine(x: ExampleModule, y: ExampleModule) = new ExampleModule {
      override def route = x.route ~ y.route
      override def swagger = x.swagger ++ y.swagger
    }
  }
}
