package ru.tinkoff.tschema.example

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.example.ExampleEnv.incrementAlohas
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.syntax._
import zio.interop.catz._
import zio._
import ru.tinkoff.tschema.finagle.zioInstance._
import ru.tinkoff.tschema.finagle.showInstances._
import cats.instances.all._

case object Greeting {
  def api =
    (operation('hello) |> $$[String]) <>
      (operation('aloha) |> $$[(String, Int)])

  object handler {
    def hello: Http[String] = ZIO.succeed("Hello")
    def aloha: Http[(String, Int)] =
      incrementAlohas.flatMap(i => ZIO.succeed(("aloha", i)))
  }

  val svc: Http[Response] = MkService[Http](api)(handler)
}
