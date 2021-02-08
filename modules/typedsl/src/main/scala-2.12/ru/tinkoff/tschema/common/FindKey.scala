package ru.tinkoff.tschema.common

import ru.tinkoff.tschema.common.Find
import ru.tinkoff.tschema.typeDSL.Key
import shapeless.tag.Tagged
import shapeless.{DepFn0, HList, Witness}
import ru.tinkoff.tschema.common.FindKey.Aux

trait FindKey[Params <: HList] extends DepFn0 {
  type Out <: String
}

object FindKey extends FindSymbolicKey {
  type Aux[Params <: HList, O <: String] = FindKey[Params] { type Out = O }

  final implicit def findStringKey[Params <: HList, name <: String](
      implicit selectKey: Find.Aux[Key, Params, name],
      keyValue: Witness.Aux[name]
  ): Aux[Params, name] = new FindKey[Params] {
    type Out = name
    def apply(): name = keyValue.value
  }
}

trait FindSymbolicKey {
  final implicit def findSymbolicKey[Params <: HList, name <: String](
      implicit selectKey: Find.Aux[Key, Params, Symbol with Tagged[name]],
      keyValue: Witness.Aux[name]
  ): Aux[Params, name] = new FindKey[Params] {
    type Out = name
    def apply(): name = keyValue.value
  }
}
