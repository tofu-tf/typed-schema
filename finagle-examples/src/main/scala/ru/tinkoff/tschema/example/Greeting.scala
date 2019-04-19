package ru.tinkoff.tschema.example
import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.finagle.{MkService, Serve}
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL.Prefix
import scalaz.zio._
import cats.instances.string._
import shapeless.{HNil, Witness}
import scalaz.zio.interop.catz._
import ru.tinkoff.tschema.finagle.showInstances._

case object Greeting {
  def api = operation('hello) |> $$[String]

  object handler {
    def hello: Http[String] = ZIO.succeed("Hello")
  }

  val svc: Http[Response] = MkService[Http](api)(handler)
}
