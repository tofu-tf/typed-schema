package ru.tinkoff.tschema
package example

import cats.Monad
import cats.instances.string._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.order._
import cats.syntax.semigroupk._
import com.twitter.finagle.http.Response
import ru.tinkoff.tschema.finagle.Serve.Filter
import ru.tinkoff.tschema.finagle.{Rejection, Routed, RoutedPlus, Serve}
import ru.tinkoff.tschema.param.ParamSource.Query
import ru.tinkoff.tschema.swagger.SwaggerMapper
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, Witness}
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.custom.syntax._
import ru.tinkoff.tschema.finagle.MkService
import ru.tinkoff.tschema.swagger.MkSwagger
import syntax._

class VersionModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import VersionModule._
  val route = MkService[H](api)(service)
  val swag  = MkSwagger(api)
}

object VersionModule {
  def api = tagPrefix("versioned") |> (
    (version("v1") |> get |> plain[String]) <>
      (version("v2") |> get |> json[Map[String, Int]]) <>
      (version("v2.1") |> get |> json[Vector[String]])
  )

  object service {
    def v1     = "Ololo"
    def v2     = Map("Olol" -> 0)
    def `v2.1` = Vector("Olo", "lo")
  }

}

final class version[v] extends DSLAtom

object version {
  def wrongVersion(shouldBe: String, passed: String) =
    Rejection.malformedParam("version", s"passed version $passed shouldBe: $shouldBe", Query)

  def apply[v](v: Witness.Aux[v]): version[v] :> Key[v] = new :>

  implicit def versionServe[v: Name, In <: HList, H[_]: Monad: RoutedPlus]: Filter[version[v], H, In] =
    Serve.checkCont[version[v], H, In] { cnt =>
      val shouldBe = Name[v].string

      import Routed._
      Routed.checkPath[H, Response](Name[v].string, cnt) <+>
        (uriParam[H, String]("version").flatMap { s =>
          reject[H, Unit](wrongVersion(shouldBe, s)).whenA(s =!= shouldBe)
        } >> cnt)
    }

  implicit def versionSwagger[v: Name]: SwaggerMapper[version[v]]                                     = SwaggerMapper[Prefix[v]].as[version[v]]
}
