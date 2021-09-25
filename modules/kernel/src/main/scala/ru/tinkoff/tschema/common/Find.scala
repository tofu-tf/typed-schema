package ru.tinkoff.tschema.common
import shapeless._

import scala.language.higherKinds

trait Find[F[_], L <: HList] {
  type Out
  def apply(l: L): F[Out]
}

sealed trait LowLevelFind    {
  implicit def findTail[F[_], L <: HList, H](implicit tail: Find[F, L]): Find.Aux[F, H :: L, tail.Out] =
    new Find[F, H :: L] {
      type Out = tail.Out
      def apply(l: H :: L): F[tail.Out] = tail(l.tail)
    }
}

object Find extends LowLevelFind {
  def apply[F[_], L <: HList](implicit find: Find[F, L]): Aux[F, L, find.Out] = find

  type Aux[F[_], L <: HList, O] = Find[F, L] { type Out = O }
  implicit def findHead[F[_], L <: HList, X]: Aux[F, F[X] :: L, X] = new Find[F, F[X] :: L] {
    type Out = X
    def apply(l: F[X] :: L): F[X] = l.head
  }
}
