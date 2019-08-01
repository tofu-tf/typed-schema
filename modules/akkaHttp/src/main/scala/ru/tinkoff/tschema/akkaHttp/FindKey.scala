package ru.tinkoff.tschema.akkaHttp
import ru.tinkoff.tschema.common.Find
import shapeless.{DepFn0, HList, Witness}

trait FindKey[Params <: HList] extends DepFn0 {
  type Out <: Symbol
}

object FindKey {
  type Aux[Params <: HList, O <: Symbol] = FindKey[Params] {type Out = O}

  implicit def findKey[Params <: HList, name <: Symbol]
  (implicit selectKey: Find.Aux[Serve.key, Params, name],
   keyValue: Witness.Aux[name]): Aux[Params, name] = new FindKey[Params] {
    type Out = name
    def apply(): name = keyValue.value
  }
}



