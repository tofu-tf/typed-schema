package ru.tinkoff.tschema.utils.json


import io.circe.{Encoder, JsonObject, KeyEncoder, ObjectEncoder}

import scala.collection.immutable.TreeMap

object circeCodecs {
  implicit def orderedMapEncoder[K: KeyEncoder, V: Encoder]: ObjectEncoder[TreeMap[K, V]] =
    ObjectEncoder.instance(tree => JsonObject.fromIterable(
      tree.view.map { case (k, v) => KeyEncoder[K].apply(k) -> Encoder[V].apply(v) }))
}
