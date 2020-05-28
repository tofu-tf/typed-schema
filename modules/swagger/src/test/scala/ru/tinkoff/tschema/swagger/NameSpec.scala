package ru.tinkoff.tschema
package swagger
import io.circe.Encoder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import ru.tinkoff.tschema.swagger.OpenApi.Method
import ru.tinkoff.tschema.syntax._

class NameSpec extends AnyFlatSpec {
  val swagger = MkSwagger(NameSpec.api).make()

  "renaming" should "change cases" in {
    val method = swagger.paths("/test-api/TEST/first")(Method.get)
    val params = method.parameters.map(_.name).toSet
    assert(method.tags === Vector("TestApi"))
    assert(params === Set("myparam", "foo"))
  }

}
object NameSpec {
  def api =
    kebab("TestApi").tagPrefix |>
      upper("test").groupPrefix |>
      snake("first").operation |>
      lower("MyParam").header[Int] |>
      renamed("foo", "bar").queryParam[String] |>
      get |>
      complete[String]
}
