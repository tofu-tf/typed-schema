package ru.tinkoff.tschema.utils.json

import io.circe._

object circeSyntax {
  implicit class ObjectEncoderOps[A](val enc: ObjectEncoder[A]) extends AnyVal {
    def mapObjWithSrc(f: (A, JsonObject) ⇒ JsonObject): ObjectEncoder[A] = ObjectEncoder.instance[A] {
      x ⇒ f(x, enc.encodeObject(x))
    }
  }
}


