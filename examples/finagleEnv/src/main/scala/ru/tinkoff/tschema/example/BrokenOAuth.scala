import ru.tinkoff.tschema.swagger.MkSwagger
import ru.tinkoff.tschema.syntax.{get, $$, oauth, operation, |>}

object BrokenOAuth extends App {
  val test = operation("test") |> get |> oauth[From, To]("paramName", conf) |> $$[String]

  case class From(id: Long, meta: String)
  case class To(userId: Long, username: String)

  MkSwagger(test)
}