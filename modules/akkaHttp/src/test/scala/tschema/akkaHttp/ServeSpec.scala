package tschema.akkaHttp

import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.http.scaladsl.server.MissingQueryParamRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import tschema.syntax
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ServeSpec extends AnyWordSpec with Matchers with ScalatestRouteTest {
  trait Small

  import syntax._
  val dsl = syntax

  val intAnswer = 42

  object handler {
    val int = 42

    def repeat(body: String, n: Int) = body * n

    def multiply(x: Long, y: Double) = f"result is ${x * y}%.2f"

    def size(args: List[Int]) = args.size

    def min(args: List[Int]) = args.min
  }

  def api = (keyPrefix('int) :> get[Int]) ~
            (keyPrefix('repeat) :> reqBody[String] :> queryParam[Int]('n) :> post[String]) ~
            (keyPrefix('multiply) :> formField[Long]('x) :> formField[Double]('y) :> post[String]) ~
            (keyPrefix('size) :> queryParams[Option[Int]]('args) :> post[Int]) ~
            (keyPrefix('min) :> queryParams[Int]('args) :> post[Int])

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

    "multiply numbers from formdata" in {
      Post(Uri("/multiply"),
           FormData(Map(
             "x" -> HttpEntity("3"),
             "y" -> HttpEntity("1.211")))) ~>
      route ~>
      check {
        responseAs[String] shouldEqual f"result is ${3.63}%.2f"
      }
    }

    "return size of empty args" in {
      Post(Uri("/size")) ~> route ~> check {
        responseAs[Int] shouldEqual 0
      }
    }

    "return size of non empty args" in {
      Post(Uri("/size").withQuery(Query(List("1", "2", "3").map("args" -> _): _*))) ~> route ~> check {
        responseAs[Int] shouldEqual 3
      }
    }

    "return min of non empty args" in {
      Post(Uri("/min").withQuery(Query(List("3", "1", "2").map("args" -> _): _*))) ~> route ~> check {
        responseAs[Int] shouldEqual 1
      }
    }

    "reject on min with empty args" in {
      Post(Uri("/min")) ~> route ~> check {
        rejection shouldEqual MissingQueryParamRejection("args")
      }
    }
  }
}
