package ru.tinkoff.tschema.named

import akka.shapeless.DepFn2
import shapeless.ops.hlist.Prepend
import shapeless.{:+:, CNil, Coproduct, HList, Inl, Inr}

@annotation.inductive
trait Distribute[prefix <: HList, sum <: Coproduct] extends DepFn2[prefix, sum] {
  def apply(prefix: prefix, sum: sum): Out
  type Out <: Coproduct
}

/**
  * Distribute `Prepend` on all members of given `Coproduct`
  */
object Distribute {
  type Aux[prefix <: HList, sum <: Coproduct, o <: Coproduct] = Distribute[prefix, sum] {type Out = o}

  def apply[p <: HList, s <: Coproduct](implicit distribute: Distribute[p, s]): Aux[p, s, distribute.Out] = distribute

  implicit def distributeCNil[prefix <: HList]: Aux[prefix, CNil, CNil] = new Distribute[prefix, CNil] {
    type Out = CNil
    def apply(prefix: prefix, sum: CNil) = sum
  }

  implicit def distributeCCons[prefix <: HList, head <: HList, tail <: Coproduct]
  (implicit prepend: Prepend[prefix, head],
   recur: Distribute[prefix, tail]): Aux[prefix, head :+: tail, prepend.Out :+: recur.Out] =
    new Distribute[prefix, head :+: tail] {
      type Out = prepend.Out :+: recur.Out
      def apply(prefix: prefix, sum: head :+: tail): Out = sum match {
        case Inl(head) ⇒ Inl(prepend(prefix, head))
        case Inr(tail) ⇒ Inr(recur(prefix, tail))
      }
    }
}
