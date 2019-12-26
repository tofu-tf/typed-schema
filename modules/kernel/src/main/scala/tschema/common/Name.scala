package tschema.common

import shapeless.Witness

trait Name[name] {
  def string: String

  def symbol: Symbol

  override def toString = string
}

trait StringName[name <: String] extends Name[name] {
  def symbol = Symbol(string)
}

trait SymbolName[name <: Symbol] extends Name[name] {
  def string = symbol.name
}

object Name {
  def apply[name](implicit name: Name[name]): Name[name] = name


  implicit def stringName[name <: String](implicit witness: Witness.Aux[name]): Name[name] = new StringName[name] {def string = witness.value}
  implicit def symbolName[name <: Symbol](implicit witness: Witness.Aux[name]): Name[name] = new SymbolName[name] {def symbol = witness.value}
}