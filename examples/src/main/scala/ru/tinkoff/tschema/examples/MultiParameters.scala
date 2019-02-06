package ru.tinkoff.tschema.examples
import ru.tinkoff.tschema.akkaHttp.{Param, ParamSource}

object MultiParameters {
  final case class User(name: String, age: Int)

  object User{
    final implicit val params: Param[ParamSource.All, User] = Param.generate[User]
  }
}
