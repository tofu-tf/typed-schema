package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.typeDSL

trait SwaggerMeta extends typeDSL.Meta

object Description {
  class Static[str] extends SwaggerMeta
  class Resource[resName] extends SwaggerMeta
  class I18n[property] extends SwaggerMeta
}

/**
  * tagging symbol for route
  */
class Tag[tag] extends SwaggerMeta



