package ru.tinkoff.tschema

import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import de.heikoseeberger.akkahttpcirce.CirceSupport._
import ru.tinkoff.tschema.akkaHttp._
import ru.tinkoff.tschema.macros.NamedImpl

class ServeSpec extends WordSpec with Matchers with ScalatestRouteTest {
  trait Small

  import syntax._
  import typeDSL._
  val dsl = typeDSL

  val intAnswer = 42

  object handler {
    val int = 42

    def repeat(body: String, n: Int) = body * n

    def multiply(x: Long, y: Double) = f"result is ${x * y}%.2f"
  }

  def api = (keyPrefix('int) :> dsl.Get[Int]) <|>
            (keyPrefix('repeat) :> ReqBody[String] :> queryParam[Int]('n) :> dsl.Post[String]) <|>
            (keyPrefix('multiply) :> formField[Long]('x) :> formField[Double]('y) :> dsl.Post[String])

  val route = api.route(handler)

  "Simple service" should {
    "return a simple int" in {
      Get("/int") ~> route ~> check {
        responseAs[Int] shouldEqual intAnswer
      }
    }

    "multiply string by n times" in {
      Post(Uri("/repeat").withQuery(Query("n" → "5")), "batman") ~> route ~> check {
        responseAs[String] shouldEqual ("batman" * 5)
      }
    }

    "multuply numbers from formdata" in {
      Post(Uri("/multiply"),
           FormData(Map(
             "x" → HttpEntity("3"),
             "y" → HttpEntity("1.211")))) ~>
      route ~>
      check {
        responseAs[String] shouldEqual f"result is ${3.63}%.2f"
      }
    }
  }
}
