package ru.tinkoff.tschema.swagger.typesafe

import ru.tinkoff.tschema.swagger.SwaggerBuilder
import ru.tinkoff.tschema.typeDSL.DSLDef

// MkSwagger wrapper with checking existence of description for parameters in typesafe-config.
// By default all missing descriptions will be reported as info.
// Enabld "-Xmacro-settings:warn-swagger-description" to report as warnings.
object MkDescribedSwagger {
  def apply[Def <: DSLDef](definition: => Def): SwaggerBuilder =
    macro MkDescribedSwaggerMacro.impl[Def]
}
