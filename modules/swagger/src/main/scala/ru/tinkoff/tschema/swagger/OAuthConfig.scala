package ru.tinkoff.tschema.swagger

case class OAuthConfig(realm: String, flows: OpenApiFlows)

trait ConfigDesc[A] {
  type realm <: String with Singleton

  val realm: String
  val flows: OpenApiFlows
}

object ConfigDesc {
  type Aux[A, realm0] = ConfigDesc[A] {
    type realm = realm0
  }

  def apply[A](implicit ev: ConfigDesc[A]): ConfigDesc[A] = ev

  implicit def makeConfig1[A <: OAuthConfig : ValueOf]: ConfigDesc[A] { val stable: A; type realm = stable.realm.type }  =
    new ConfigDesc[A] {
      val stable = valueOf[A]
      override type realm = stable.realm.type

      override val realm: String = stable.realm
      override val flows: OpenApiFlows = stable.flows
    }
}
