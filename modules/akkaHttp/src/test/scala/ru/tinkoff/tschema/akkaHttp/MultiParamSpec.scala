package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.server.{Directives, MissingQueryParamRejection}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FlatSpec, Matchers}
import ru.tinkoff.tschema.akkaHttp.MultiParamSpec.{Page, route}
import ru.tinkoff.tschema.param.HttpParam
import ru.tinkoff.tschema.syntax
import shapeless.Witness

import scala.language.reflectiveCalls

class MultiParamSpec extends FlatSpec with Matchers with ScalatestRouteTest {
  "Multi parameter case class" should "require first param" in {
    Get("/required") ~> route ~> check {
      rejections should contain(MissingQueryParamRejection("from"))
    }
  }

  it should "require second param" in {
    Get("/required?from=3") ~> route ~> check {
      rejections should contain(MissingQueryParamRejection("to"))
    }
  }

  it should "not require optional field" in {
    Get("/required?from=3&to=5") ~> route ~> check {
      responseAs[String] shouldBe Page(3, 5).toString
    }
  }

  it should "supply optional field" in {
    Get("/required?from=3&to=5&hmm=true") ~> route ~> check {
      responseAs[String] shouldBe Page(3, 5, Some(true)).toString
    }
  }

  it should "not require whole optional record" in {
    Get("/optional?from=3") ~> route ~> check {
      responseAs[String] shouldBe None.toString
    }
    Get("/optional?to=3") ~> route ~> check {
      responseAs[String] shouldBe None.toString
    }
    Get("/optional") ~> route ~> check {
      responseAs[String] shouldBe None.toString
    }
  }

  it should "supply optional record" in {
    Get("/optional?from=3&to=5&hmm=false") ~> route ~> check {
      responseAs[String] shouldBe Some(Page(3, 5, Some(false))).toString
    }
  }

  it should "supply partial optional record with optional fields" in {
    Get("/optional?from=3&to=5") ~> route ~> check {
      responseAs[String] shouldBe Some(Page(3, 5)).toString
    }
  }


}

object MultiParamSpec {
  val page = Witness.`"page"`
  final case class Page(from: Int, to: Long, hmm: Option[Boolean] = None)
  object Page {
    implicit val pageParam: HttpParam[Page] = HttpParam.generate
  }

  val api = {
    import syntax._
    (operation("required") |> queryParam[Page]('page) |> complete[String]) <|>
      (operation('optional) |> queryParam[Option[Page]]("page") |> complete[String])
  }
  val route = MkRoute(api)(new {
    def required(page: Page): String         = page.toString
    def optional(page: Option[Page]): String = page.toString
  })

  import Directives._
  val kek = Directives.parameter("kek".as[Option[String]])( os => complete(os) )

  parameters(("raw".as[Boolean], "offset".as[Int], "pageSize".as[Int]))
}
