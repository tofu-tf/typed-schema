package ru.tinkoff.tschema
package akkaHttp

import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpResponse}
import akka.http.scaladsl.server.MethodRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import tschema.syntax._

import scala.concurrent.Future
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class ResultSuite extends AsyncWordSpec with ScalatestRouteTest with Matchers {
  object hello {
    @volatile var testCalled = false

    def test: String = {
      testCalled = true
      "Hello, world!"
    }

    def testAsync: Future[String] = {
      Future.successful("Hello, async world!")
    }

    def testCapture(name: String, age: Int): String = {
      testCalled = true
      s"$name, age $age"
    }
  }

  def helloApi = (keyPrefix("test") |> get |> $$[String]) <> (keyPrefix("testAsync") |> get |> $$[String]) <>
      (keyPrefix("testCapture") |> get |> capture[String]("name") |> capture[Int]("age") |> $$[String])

  val helloRoute = MkRoute(helloApi)(hello)

  "test route" should {
    "serve sync world" in {
      hello.testCalled = false
      Get("/test") ~> helloRoute ~> check {
        responseAs[String] shouldEqual "Hello, world!"
        hello.testCalled shouldBe true
      }
    }

    "serve async world" in {
      hello.testCalled = false
      Get("/testAsync") ~> helloRoute ~> check {
        responseAs[String] shouldEqual "Hello, async world!"
        hello.testCalled shouldBe false
      }
    }

    "serve capture" in {
      hello.testCalled = false
      Get("/testCapture/Bilbo/131") ~> helloRoute ~> check {
        responseAs[String] shouldEqual "Bilbo, age 131"
        hello.testCalled shouldBe true
      }
    }

    "reject bad http method" in (Post("/test") ~> helloRoute ~> check {
      handled shouldBe false

      rejection shouldBe MethodRejection(HttpMethods.GET)
    })

    "reject prefix" in (Get("/test1") ~> helloRoute ~> check {
      handled shouldBe false

      rejections shouldBe Nil
    })
  }

  object methods {
    def getting: Unit    = ()
    def posting: Unit    = ()
    def putting: Unit    = ()
    def deleting: Unit   = ()
    def completing: Unit = ()
  }

  implicit val unitAsPlainText: ToResponseMarshaller[Unit] =
    Marshaller.strict(_ => Marshalling.WithFixedContentType(ContentTypes.NoContentType, () => HttpResponse()))

  def methodApi =
    (keyPrefix("getting") |> get[Unit]) <>
      (keyPrefix("putting") |> put[Unit]) <>
      (keyPrefix("posting") |> post[Unit]) <>
      (keyPrefix("deleting") |> delete[Unit]) <>
      (keyPrefix("completing") |> $$[Unit])

  val methodRoute = MkRoute(methodApi)(methods)

  "method route" when {
    "method declared as post" should {
      "complete post" in (Post("/posting") ~> methodRoute ~> check {
        handled shouldBe true
      })
      "reject get" in (Get("/posting") ~> methodRoute ~> check {
        handled shouldBe false
        rejection shouldBe MethodRejection(HttpMethods.POST)
      })
    }

    "method declared as get" should {
      "complete get" in (Get("/getting") ~> methodRoute ~> check {
        handled shouldBe true
      })
      "reject post" in (Post("/getting") ~> methodRoute ~> check {
        handled shouldBe false
        rejection shouldBe MethodRejection(HttpMethods.GET)
      })
    }

    "method declared via complete" should {
      "complete get" in (Get("/completing") ~> methodRoute ~> check {
        handled shouldBe true
      })
      "complete post" in (Post("/completing") ~> methodRoute ~> check {
        handled shouldBe true
      })
    }
  }

}
