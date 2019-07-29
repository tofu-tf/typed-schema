package ru.tinkoff.tschema.utils.json

import io.circe._

object circeSyntax {
  implicit class ObjectEncoderOps[A](val enc: Encoder.AsObject[A]) extends AnyVal {
    def mapObjWithSrc(f: (A, JsonObject) => JsonObject): Encoder.AsObject[A] = Encoder.AsObject.instance[A] {
      x => f(x, enc.encodeObject(x))
    }
  }
}


