package ru.tinkoff.tschema
package example

import ru.tinkoff.tschema.finagle.Serve.Filter
import ru.tinkoff.tschema.finagle.{MkService, Rejection, Routed, RoutedPlus, Serve}
import ru.tinkoff.tschema.param.ParamSource.Query
import ru.tinkoff.tschema.swagger.{SwaggerMapper, _}
import syntax._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, Witness}
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.semigroupk._
import cats.syntax.order._
import cats.instances.string._
import ru.tinkoff.tschema.common.Name
import cats.instances.string._
import Routed.{reject, uriParam}
import cats.{Monad, SemigroupK}
import com.twitter.finagle.http.{Request, Response}
import ru.tinkoff.tschema.param.Param
import ru.tinkoff.tschema.custom.syntax._

class VersionModule[H[_]: Monad: RoutedPlus] extends ExampleModule[H] {
  import VersionModule._
  val route = MkService[H](api)(service)
  val swag = MkSwagger(api)
}

object VersionModule {
  def api = tagPrefix('versioned) |> (
    (version('v1) |> get |> plain[String]) <>
      (version('v2) |> get |> json[Map[String, Int]]) <>
      (version("v2.1") |> get |> json[Vector[String]])
  )

  object service {
    def v1 = "Ololo"
    def v2 = Map("Olol" -> 0)
    def `v2.1` = Vector("Olo", "lo")
  }

}

final class version[v] extends DSLAtom

object version {
  def wrongVersion(shouldBe: String, passed: String) =
    Rejection.malformedParam("version", s"passed version $passed shouldBe: $shouldBe", Query)

  def apply[v](v: Witness.Aux[v]): version[v] :> Key[v] = new version[v] :> key(v)

  implicit def versionServe[v: Name, In <: HList, H[_]: Monad: RoutedPlus]: Filter[version[v], H, In] =
    Serve.checkCont[version[v], H, In] { cnt =>
      val shouldBe = Name[v].string

      Routed.checkPath[H, Response](Name[v].string, cnt) <+>
        (uriParam[H, String]("version").flatMap { s =>
          reject[H, Unit](wrongVersion(shouldBe, s)).whenA(s =!= shouldBe)
        } >> cnt)
    }

  implicit def versionSwagger[v: Name]: SwaggerMapper[version[v]] = SwaggerMapper[Prefix[v]].as[version[v]]
}
