package ru.tinkoff.tschema.swagger

import scala.collection.immutable.TreeMap

trait OAuthConfigInstances {
  implicit def makeConfigDesc[A <: OAuthConfig: ValueOf]
      : ConfigDesc[A] { val stable: A; type realm = stable.realm.type } =
    new ConfigDesc[A] {
      val stable = valueOf[A]
      override type realm = stable.realm.type

      override val realm: String                       = stable.realm
      override val flows: TreeMap[String, OpenApiFlow] = stable.flows
    }
}
