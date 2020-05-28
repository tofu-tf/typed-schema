package ru.tinkoff.tschema.common

import shapeless.Witness

final class Name[name](val string: String) extends AnyVal {
  def symbol: Symbol = Symbol(string)

  override def toString = string
}

object Name {
  def apply[name](implicit name: Name[name]): Name[name] = name

  implicit def stringName[name <: String](implicit witness: Witness.Aux[name]): Name[name] =
    new Name[name](witness.value)

  implicit def symbolName[name <: Symbol](implicit witness: Witness.Aux[name]): Name[name] =
    new Name[name](witness.value.name)
}
