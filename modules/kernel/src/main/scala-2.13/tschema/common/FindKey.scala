package tschema.common

import ru.tinkoff.tschema.common.Find
import ru.tinkoff.tschema.typeDSL.Key
import shapeless.{DepFn0, HList}
import tschema.common.Name

trait FindKey[Params <: HList] extends DepFn0 {
  type Out <: String
}

object FindKey{
  type Aux[Params <: HList, O <: String] = FindKey[Params] { type Out = O }

  implicit def findStringKey[Params <: HList, name <: String](
      implicit selectKey: Find.Aux[Key, Params, name],
      keyValue: ValueOf[name]
  ): Aux[Params, name] = new FindKey[Params] {
    type Out = name
    def apply(): name = keyValue.value
  }
}
