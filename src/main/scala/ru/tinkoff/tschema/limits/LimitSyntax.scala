package ru.tinkoff.tschema.limits
import ru.tinkoff.tschema.typeDSL.DSLAtom
import shapeless.{::, HList, HNil, Witness}

trait LimitSyntax {
  def limit[count <: Int](rate: ⇒ Witness.Lt[count]) = new MkLimit[count]

  class MkLimit[count <: Int] {
    def /[unit <: TimeUnit](unit: unit) = new Limit[HNil, Rate[count, unit]]
  }

  implicit class LimitsSyntax[L <: HList, rate <: Rate[_, _]](lst: ⇒ Limit[L, rate]) {
    def ![x](witness: Witness.Lt[x]): Limit[x :: L, rate] = new Limit[x :: L, rate]
  }
}
