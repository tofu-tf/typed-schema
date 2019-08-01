package ru.tinkoff.tschema
package example
import ru.tinkoff.tschema.finagle.tethysInstances._
import ru.tinkoff.tschema.finagle.unitInstance._
import ru.tinkoff.tschema.finagle.{MkService, NoneCompleting, StringCompleting}
import ru.tinkoff.tschema.swagger.MkSwagger
import syntax._
import zio.ZIO

sealed trait Receive[+A]

object Receive {
  implicit def decompose[A] =
    Decompose.make[Receive[A]] { case n @ NotFound => n } { case n @ BadKey(_) => n } { case Result(a) => a } result
}

case object NotFound extends NoneCompleting(404) with Receive[Nothing]

final case class BadKey(s: String) extends Receive[Nothing]

object BadKey extends StringCompleting[BadKey]({ case BadKey(key) => s"key $key is baaaad" }, 404)

final case class Result[A](x: A) extends Receive[A]

object ReceiveModule extends ExampleModule {
  def api =
    tagPrefix("storage") |> queryParam[String]("key") |> ((
      opPut |> body[String]('value) |> $$[Unit]
    ) <> (
      opGet |> $$[Composite[Receive[String]]]
    ))

  def route = MkService[Http](api)(ReceiveService)
  def swag  = MkSwagger(api)
}

object ReceiveService {
  def put(key: String, value: String): Example[Unit] = ZIO.accessM(_.storage.update(_ + (key -> value)).unit)
  def get(key: String): Example[Receive[String]] =
    if (key.isEmpty || key.startsWith("bad")) ZIO.succeed(BadKey(key))
    else ZIO.accessM(_.storage.get.map(_.get(key).fold[Receive[String]](NotFound)(Result(_))))
}
