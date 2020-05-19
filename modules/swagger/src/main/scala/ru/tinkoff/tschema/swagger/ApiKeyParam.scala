package ru.tinkoff.tschema
package swagger
import ru.tinkoff.tschema.typeDSL.CanHoldApiKey
import ru.tinkoff.tschema.typeDSL._

final case class ApiKeyParam[Param <: CanHoldApiKey, name, x](in: OpenApiParam.In)

object ApiKeyParam {
  implicit def header[name, x]: ApiKeyParam[Header[name, x], name, x]    = ApiKeyParam(OpenApiParam.In.header)
  implicit def query[name, x]: ApiKeyParam[QueryParam[name, x], name, x] = ApiKeyParam(OpenApiParam.In.query)
  implicit def cookie[name, x]: ApiKeyParam[Cookie[name, x], name, x]    = ApiKeyParam(OpenApiParam.In.cookie)
}
