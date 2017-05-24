package ru.tinkoff.tschema.common

import akka.shapeless.DepFn2
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Prepend

trait DistributeLab[prefix <: HList, sum <: Coproduct] extends DepFn2[prefix, sum] {
  def apply(prefix: prefix, sum: sum): Out
  type Out <: Coproduct
}

/**
  * Distribute `Prepend` on all members of given labelled `Coproduct`
  */
object DistributeLab {
  type Aux[prefix <: HList, sum <: Coproduct, o <: Coproduct] = DistributeLab[prefix, sum] {type Out = o}

  def apply[p <: HList, s <: Coproduct](implicit distribute: DistributeLab[p, s]): Aux[p, s, distribute.Out] = distribute

  implicit def distributeCNil[prefix <: HList]: Aux[prefix, CNil, CNil] = new DistributeLab[prefix, CNil] {
    type Out = CNil
    def apply(prefix: prefix, sum: CNil) = sum
  }

  implicit def distributeCCons[prefix <: HList, key, value <: HList, tail <: Coproduct]
  (implicit prepend: Prepend[prefix, value],
   recur: DistributeLab[prefix, tail]): Aux[prefix, FieldType[key, value] :+: tail, FieldType[key, prepend.Out] :+: recur.Out] =
    new DistributeLab[prefix, FieldType[key, value] :+: tail] {
      type Out = FieldType[key, prepend.Out] :+: recur.Out
      def apply(prefix: prefix, sum: FieldType[key, value] :+: tail): Out = sum match {
        case Inl(head) ⇒ Inl(labelled.field[key](prepend(prefix, head)))
        case Inr(tail) ⇒ Inr(recur(prefix, tail))
      }
    }
}
