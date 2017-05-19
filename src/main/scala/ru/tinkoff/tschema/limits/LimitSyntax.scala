package ru.tinkoff.tschema.limits
import ru.tinkoff.tschema.typeDSL.DSLAtom
import shapeless.{::, HList, HNil, Witness}

trait LimitSyntax {
  val limit = new Limit[HNil]

  implicit class LimitsSyntax[L <: HList](lst: ⇒ Limit[L]) {
    def ![x](witness: Witness.Lt[x]): Limit[x :: L] = new Limit[x :: L]
  }
}
