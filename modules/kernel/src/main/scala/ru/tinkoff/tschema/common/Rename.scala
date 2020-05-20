package ru.tinkoff.tschema.common

import ru.tinkoff.tschema.NamedSyntaxOps
import ru.tinkoff.tschema.utils.transform

class Rename[name, R] extends NamedSyntaxOps[Rename[name, R]]

object Rename {
  implicit def renamed[name, R](implicit trans: TransformName[R], name: Name[name]): Name[Rename[name, R]] =
    new Name[Rename[name, R]](trans.transform(name.string))

  class snakeCase
  class kebabCase
  class lowerCase
  class upperCase
}

trait TransformName[R] {
  def transform(name: String): String
}

object TransformName {
  implicit val snakeCase: TransformName[Rename.snakeCase] = transform.snakeCase
  implicit val kebabCase: TransformName[Rename.kebabCase] = transform.kebabCase
  implicit val lowerCase: TransformName[Rename.lowerCase] = _.toLowerCase
  implicit val upperCase: TransformName[Rename.upperCase] = _.toUpperCase
}
