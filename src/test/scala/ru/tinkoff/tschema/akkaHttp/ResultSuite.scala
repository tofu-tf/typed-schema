package ru.tinkoff.tschema

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpResponse}
import akka.http.scaladsl.server.MethodRejection
import akka.http.scaladsl.server.RouteResult.Rejected
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.ActorMaterializer
import org.scalatest.{AsyncFlatSpec, AsyncWordSpec, Matchers}
import ru.tinkoff.tschema.akkaHttp.MkRoute

import scala.concurrent.Future
import ru.tinkoff.tschema.syntax
import syntax._

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
  }

  def helloApi = (keyPrefix('test) |> get[String]) <> (keyPrefix('testAsync) |> get[String])

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
    (keyPrefix('getting) |> get[Unit]) <>
      (keyPrefix('putting) |> put[Unit]) <>
      (keyPrefix('posting) |> post[Unit]) <>
      (keyPrefix('deleting) |> delete[Unit]) <>
      (keyPrefix('completing) |> syntax.complete[Unit])

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
