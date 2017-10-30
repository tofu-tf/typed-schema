package ru.tinkoff

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.MethodRejection
import akka.http.scaladsl.server.RouteResult.Rejected
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.ActorMaterializer
import org.scalatest.{AsyncFlatSpec, Matchers}
import ru.tinkoff.tschema.akkaHttp.MkRoute

import scala.concurrent.Future
import ru.tinkoff.tschema.syntax._



class ResultSuite extends AsyncFlatSpec with ScalatestRouteTest with Matchers{
  object handler {
    def test: String = {
      "Hello, world!"
    }

    def testAsync: Future[String] = {
      Future.successful("Hello, async world!")
    }
  }

  def api = (keyPrefix('test) |> get[String]) <> (keyPrefix('testAsync) |> get[String])

  val testRoute = MkRoute(api)(handler)

  "test route" should "serve sync world" in (Get("/test") ~> testRoute ~> check {
    responseAs[String] shouldEqual "Hello, world!"
  })

  it should "serve async world" in  (Get("/testAsync") ~> testRoute ~> check {
    responseAs[String] shouldEqual "Hello, async world!"
  })

  it should "reject bad http method" in  (Post("/test") ~> testRoute ~> check {
    handled shouldBe false

    rejection shouldBe MethodRejection(HttpMethods.GET)
  })

  it should "reject prefix" in  (Post("/test1") ~> testRoute ~> check {
    handled shouldBe false

    rejections shouldBe Nil
  })
}
