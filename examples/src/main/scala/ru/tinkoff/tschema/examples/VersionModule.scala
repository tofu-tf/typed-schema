package ru.tinkoff.tschema.examples

import akka.http.scaladsl.server.{Directive, Rejection}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.swagger.{SwaggerMapper, _}
import syntax._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, Witness}

object VersionModule extends ExampleModule {
  def api = tagPrefix('versioned) |> (
    (version('v1) |> get[String]) <>
      (version('v2) |> get[Map[String, Int]]) <>
      (version(Symbol("v2.1")) |> get[Vector[String]])
    )

  object service {
    def v1 = "Ololo"
    def v2 = Map("Olol" -> 0)
    def `v2.1` = Vector("Olo", "lo")
  }

  val route = MkRoute(api)(service)
  val swag = api.mkSwagger
}

final class version[v] extends DSLAtom

object version {

  import akka.http.scaladsl.server.Directives._

  case class WrongVersionRejection(shouldBe: String, passed: String) extends Rejection

  def apply[v <: Symbol](v: Witness.Lt[v]): version[v] :> Key[v] = new version[v] :> key(v)

  implicit def versionServe[v <: Symbol, In <: HList](implicit w: Witness.Aux[v]): Serve.Aux[version[v], In, In] = Serve.serveCheck {
    Directive { f =>
      parameter("version") { v =>
        if (v == w.value.name) f(())
        else reject(WrongVersionRejection(w.value.name, v))
      } ~
        pathPrefix(w.value.name) {
          f(())
        }
    }
  }
  implicit def versionSwagger[v <: Symbol](implicit w: Witness.Aux[v]): SwaggerMapper[version[v]] = SwaggerMapper[Prefix[v]].as[version[v]]
}
