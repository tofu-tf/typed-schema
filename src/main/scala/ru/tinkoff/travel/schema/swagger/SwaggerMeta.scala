package ru.tinkoff.travel.schema.swagger

import ru.tinkoff.travel.schema.typeDSL

trait SwaggerMeta extends typeDSL.Meta

object Description {
  class Static[str] extends SwaggerMeta
  class Resource[resName] extends SwaggerMeta
  class I18n[property] extends SwaggerMeta
}

class Tag[tag] extends SwaggerMeta


