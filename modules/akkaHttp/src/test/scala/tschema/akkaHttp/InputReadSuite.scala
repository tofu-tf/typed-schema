package tschema
package akkaHttp
import akka.http.scaladsl.testkit.ScalatestRouteTest
import shapeless.{HList, HNil, Witness}
import shapeless.ops.hlist.Selector
import akka.http.scaladsl.server.Directives.provide
import tschema.typeDSL.DSLAtom
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputReadSuite extends AnyFlatSpec with Matchers with ScalatestRouteTest {
  import InputReadSuite._
  val route = MkRoute.of(api)(handler)(MagicalInput("Sergey") :: HNil)

  "generated route" should "answer with hello" in {
    Get("/magic") ~> route ~> check {
      responseAs[String] shouldBe "Hello, Sergey!"
    }
  }

}

final case class MagicalInput(value: String)

final case class magicParam[name <: Symbol](name: Witness.Aux[name]) extends DSLAtom

object magicParam {
  implicit def serve[In <: HList, name <: Symbol](
      implicit sel: Selector[In, MagicalInput]): Serve.Add[magicParam[name], In, name, MagicalInput] =
    Serve.serveAddIn(in => provide(sel(in)))
}

object InputReadSuite {
  import tschema.typeDSL._
  import syntax._
  def api = get |> operation('magic) |> magicParam('hello) |> $$[String]
  object handler {
    def magic(hello: MagicalInput) = s"Hello, ${hello.value}!"
  }
}
