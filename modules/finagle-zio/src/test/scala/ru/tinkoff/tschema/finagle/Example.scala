package ru.tinkoff.tschema
package finagle

import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.finagle.Example.Http
import typeDSL._
import shapeless.{HNil, Witness}
import ru.tinkoff.tschema.finagle.zioInstance.{ZIOHttp, zioRouted, zioRunnable}
import zio.ZIO
final case class Example(trackingId: String)
import syntax._
import zio.interop.catz._
import showInstances._
import cats.instances.string._

object Example {
  type Http[+A] = ZIOHttp[Example, Nothing, A]
  type Exec[+A] = ZIO[Example, Nothing, A]
  final implicit val httpRouted: Routed[Http]      = zioRouted
  final implicit val httpRun: Runnable[Http, Exec] = zioRunnable()
}

case object Greeting {
  def api =
    operation('hello) |> $$[String]

  object handler {
    def hello: Http[String] = ZIO.succeed("Hello")
  }

  implicitly[Serve[Prefix[Witness.`'hello`.T], Http, HNil, HNil]]

  val svc: Http[Response] = MkService[Http](api)(handler)
}
