package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{HList, HNil, Witness}
import shapeless.ops.hlist.Selector
import akka.http.scaladsl.server.Directives.provide
import ru.tinkoff.tschema.typeDSL.DSLAtom

class InputReadSuite extends FlatSpec with Matchers with ScalatestRouteTest {
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
  import ru.tinkoff.tschema.typeDSL._
  import ru.tinkoff.tschema.syntax._
  def api = get |> operation('magic) |> magicParam('hello) |> $$[String]
  object handler {
    def magic(hello: MagicalInput) = s"Hello, ${hello.value}!"
  }
}
