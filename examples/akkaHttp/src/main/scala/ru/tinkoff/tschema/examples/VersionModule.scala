package ru.tinkoff.tschema
package examples

import akka.http.scaladsl.server.{Directive, Rejection}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import ru.tinkoff.tschema.akkaHttp.{MkRoute, Serve}
import ru.tinkoff.tschema.swagger.{SwaggerMapper, _}
import syntax._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, Witness}
import ru.tinkoff.tschema.common.Name

object VersionModule extends ExampleModule {
  def api = tagPrefix("versioned") |> (
    (version("v1") |> get[String]) <>
      (version("v2") |> get[Map[String, Int]]) <>
      (version(Symbol("v2.1")) |> get[Vector[String]])
    )

  object service {
    def v1 = "Ololo"
    def v2 = Map("Olol" -> 0)
    def `v2.1` = Vector("Olo", "lo")
  }

  val route = MkRoute(api)(service)
  val swag = MkSwagger(api)
}

final class version[v] extends DSLAtom

object version {

  import akka.http.scaladsl.server.Directives._

  case class WrongVersionRejection(shouldBe: String, passed: String) extends Rejection

  def apply[v](v: Witness.Aux[v]): version[v] :> Key[v] = new :>

  implicit def versionServe[v : Name, In <: HList]: Serve.Aux[version[v], In, In] = Serve.serveCheck {
    Directive { f =>
      parameter("version") { v =>
        if (v == Name[v].string) f(())
        else reject(WrongVersionRejection(Name[v].string, v))
      } ~
        pathPrefix(Name[v].string) {
          f(())
        }
    }
  }
  implicit def versionSwagger[v: Name]: SwaggerMapper[version[v]] = SwaggerMapper[Prefix[v]].as[version[v]]
}
