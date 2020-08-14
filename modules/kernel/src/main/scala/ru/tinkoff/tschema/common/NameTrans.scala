package ru.tinkoff.tschema.common

import ru.tinkoff.tschema.NamedSyntaxOps
import ru.tinkoff.tschema.utils.transform

class NameTrans[name, R] extends NamedSyntaxOps[NameTrans[name, R], name]

object NameTrans {
  implicit def renamed[name, R](implicit trans: TransformName[R], name: Name[name]): Name[NameTrans[name, R]] =
    new Name[NameTrans[name, R]](trans.transform(name.string))

  class snakeCase
  class kebabCase
  class lowerCase
  class upperCase
}

trait TransformName[R] {
  def transform(name: String): String
}

object TransformName {
  implicit val snakeCase: TransformName[NameTrans.snakeCase] = transform.snakeCase
  implicit val kebabCase: TransformName[NameTrans.kebabCase] = transform.kebabCase
  implicit val lowerCase: TransformName[NameTrans.lowerCase] = _.toLowerCase
  implicit val upperCase: TransformName[NameTrans.upperCase] = _.toUpperCase
}
