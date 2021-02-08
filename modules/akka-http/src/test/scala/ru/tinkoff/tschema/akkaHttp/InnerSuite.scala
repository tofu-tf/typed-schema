package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpResponse}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import ru.tinkoff.tschema.syntax._

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.reflectiveCalls

class InnerSuite extends AsyncFlatSpec with ScalatestRouteTest with Matchers {

  object impl {
    object first {
      def get: String           = "first"
      def post(message: String) = s"first $message"
    }
    val second = new {
      def get: String           = "second"
      def post(message: String) = s"second $message"
    }
  }

  implicit val unitAsPlainText: ToResponseMarshaller[Unit] =
    Marshaller.strict(_ => Marshalling.WithFixedContentType(ContentTypes.NoContentType, () => HttpResponse()))

  def api =
    (
      groupPrefix("first") |> ((
        opGet |> $$[String]
      ) <> (
        opPost |> queryParam[String]("message") |> $$[String]
      ))
    ) <> (
      groupPrefix("second") |> ((
        opGet |> $$[String]
      ) <> (
        opPost |> body[String]("message") |> $$[String]
      ))
    )

  val route = MkRoute(api)(impl)

  "first group" should "handle get" in Get("/first") ~> route ~> check {
    responseAs[String] shouldBe "first"
  }

  it should "handle post" in Post("/first?message=hello+oleg") ~> route ~> check {
    responseAs[String] shouldBe "first hello oleg"
  }

  "second group" should "handle get" in Get("/second") ~> route ~> check {
    responseAs[String] shouldBe "second"
  }

  it should "handle post" in Post("/second", "hello oleg") ~> route ~> check {
    responseAs[String] shouldBe "second hello oleg"
  }

}
