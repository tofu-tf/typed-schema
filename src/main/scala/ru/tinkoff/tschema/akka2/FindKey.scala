package ru.tinkoff.tschema.akka2
import akka.shapeless.DepFn1
import shapeless.{DepFn0, HList, Witness}
import shapeless.ops.record.Selector

trait FindKey[Params <: HList] extends DepFn0 {
  type Out <: Symbol
}

object FindKey {
  type Aux[Params <: HList, O <: Symbol] = FindKey[Params] {type Out = O}

  implicit def findKey[Params <: HList, O <: Symbol]
  (implicit selectKey: Selector[Params, Serve.key],
   keyValue: Witness.Aux[O]): Aux[Params, Symbol] = new FindKey[Params] {
    type Out = Symbol
    def apply(): Symbol = 'asdasd
  }
}



