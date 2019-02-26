package ru.tinkoff.tschema

import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.akkaHttp._

class ServeSpec extends WordSpec with Matchers with ScalatestRouteTest {
  trait Small

  import syntax._
  import typeDSL._
  val dsl = syntax

  val intAnswer = 42

  object handler {
    val int = 42

    def repeat(body: String, n: Int) = body * n

    def multiply(x: Long, y: Double) = f"result is ${x * y}%.2f"
  }

  def api = (keyPrefix('int) :> get[Int]) ~
            (keyPrefix('repeat) :> reqBody[String] :> queryParam[Int]('n) :> post[String]) ~
            (keyPrefix('multiply) :> formField[Long]('x) :> formField[Double]('y) :> post[String])

  val route = MkRoute(api)(handler)

  "Simple service" should {
    "return a simple int" in {
      Get("/int") ~> route ~> check {
        responseAs[Int] shouldEqual intAnswer
      }
    }

    "multiply string by n times" in {
      Post(Uri("/repeat").withQuery(Query("n" -> "5")), "batman") ~> route ~> check {
        responseAs[String] shouldEqual ("batman" * 5)
      }
    }

    "multuply numbers from formdata" in {
      Post(Uri("/multiply"),
           FormData(Map(
             "x" -> HttpEntity("3"),
             "y" -> HttpEntity("1.211")))) ~>
      route ~>
      check {
        responseAs[String] shouldEqual f"result is ${3.63}%.2f"
      }
    }
  }
}
