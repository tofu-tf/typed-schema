package ru.tinkoff.tschema.finagle.zioRouting

import com.twitter.finagle.http.{Request, Response}
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.tschema.finagle.{MkService, Serve}
import ru.tinkoff.tschema.finagle.showInstances._
import ru.tinkoff.tschema.syntax._
import ru.tinkoff.tschema.typeDSL.DSLAtom
import shapeless.HList
import zio.Exit.Success
import zio.{BootstrapRuntime, Exit, UIO}
import zio.interop.catz._

class ResultSuite extends AnyWordSpec with BootstrapRuntime {
  type F[+A] = UIOHttp[A]

  @volatile var someAtomServed: Boolean = false

  def someAtom: SomeAtom = null

  class SomeAtom extends DSLAtom

  object SomeAtom {
    implicit def serve[In <: HList]: Serve[SomeAtom, F, In, In] =
      (in, f) => UIO.effectTotal { someAtomServed = true } *> f(in)
  }

  def testSomeAtom =
    get :>
      keyPrefix("testSomeAtom") :>
      someAtom :>
      $$[String]

  def testCapture =
    get :>
      prefix("test") :>
      prefix("capture") :>
      key("testCapture") :>
      capture[String]("id") :>
      $$[String]

  def api = testSomeAtom <|> testCapture

  object handler {
    def testSomeAtom: String =
      "success"

    def testCapture(id: String): String =
      s"captured $id"
  }

  val service: F[Response] = MkService[F](api)(handler)

  def runSync(uri: String): Exit[Fail[NoError], String] = {
    val req     = Request(uri)
    val routing = ZioRouting(req, req.path, matched = 0, embedded = ())
    unsafeRunSync(service.provide(routing).map(_.contentString))
  }

  "serving some atom" should {
    "be succeed if path is correct" in {
      someAtomServed = false
      assert(runSync("testSomeAtom") == Success("success"))
      assert(someAtomServed)
    }

    "not be redundant if url path is incorrect" in {
      someAtomServed = false
      assert(runSync("testSomeAtom1") != Success("success"))
      assert(!someAtomServed)
    }
  }

  "serving capture" should {
    "be succeed if it non-empty" in {
      assert(runSync("test/capture/13") == Success("captured 13"))
    }

    "fail on empty string" in {
      assert(runSync("test/capture/") != Success("captured "))
    }
  }
}
