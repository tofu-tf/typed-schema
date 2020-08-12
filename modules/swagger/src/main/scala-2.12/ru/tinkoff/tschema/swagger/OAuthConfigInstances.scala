package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.swagger.{ConfigDesc, OAuthConfig, OpenApiFlow}
import shapeless.Witness

import scala.collection.immutable.TreeMap

trait OAuthConfigInstances {
  implicit def makeConfigDesc[A <: OAuthConfig: Witness.Aux]
      : ConfigDesc[A] { val stable: A; type realm = stable.realm.type } =
    new ConfigDesc[A] {
      val stable: A = implicitly[Witness.Aux[A]].value
      override type realm = stable.realm.type

      override val realm: String                       = stable.realm
      override val flows: TreeMap[String, OpenApiFlow] = stable.flows
    }
}
