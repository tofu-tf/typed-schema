package ru.tinkoff.tschema.swagger

import scala.collection.immutable.TreeMap

case class OAuthConfig(realm: String, flows: TreeMap[String, OpenApiFlow] = TreeMap.empty) {
  def flow(flow: OpenApiFlow): OAuthConfig = copy(realm, flows.updated(flow.name, flow))
}

object OAuthConfig extends OAuthConfigInstances

trait ConfigDesc[A] {
  type realm <: String with Singleton

  val realm: String
  val flows: TreeMap[String, OpenApiFlow]
}

object ConfigDesc {
  type Aux[A, realm0] = ConfigDesc[A] {
    type realm = realm0
  }

  def apply[A](implicit ev: ConfigDesc[A]): ConfigDesc[A] = ev
}
