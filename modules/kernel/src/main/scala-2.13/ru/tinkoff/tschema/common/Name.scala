package ru.tinkoff.tschema.common

final class Name[name](val string: String) {
  def symbol: Symbol = Symbol(string)

  override def toString = string
}

object Name extends SymbolName {
  def apply[name](implicit name: Name[name]): Name[name] = name

  implicit def stringName[name <: String with Singleton](implicit value: ValueOf[name]): Name[name] =
    new Name[name](value.value)
}
trait SymbolName {
  implicit def symbolName[name <: String with Singleton](
      implicit value: ValueOf[name]
  ): Name[Symbol with shapeless.tag.Tagged[name]] =
    new Name(value.value)
}
