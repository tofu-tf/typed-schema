package ru.tinkoff.tschema.common

import shapeless.Witness

trait Name[name] {
  def string: String
  override def toString = string
}

object Name {
  def apply[name](implicit name: Name[name]): Name[name] = name

  implicit def stringName[name <: String](implicit witness: Witness.Aux[name]): Name[name] =
    new Name[name] {
      val string = witness.value
    }

  implicit def symbolName[name <: Symbol](implicit witness: Witness.Aux[name]): Name[name] =
    new Name[name] {
      val string: String = witness.value.name
    }
}
