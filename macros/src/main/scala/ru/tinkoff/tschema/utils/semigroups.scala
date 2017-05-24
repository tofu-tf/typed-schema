package ru.tinkoff.tschema.utils
import cats.SemigroupK
import cats.kernel.{Order, Semigroup}
import cats.syntax.order._
import shapeless.newtype

import Numeric.Implicits._

trait SemigroupNewtype {
  abstract final class Tag
  type T[X] = newtype.Newtype[X, Tag]

  def apply[X](x: X): T[X] = newtype(x)

  def combine[X](x: X, y: X): X

  implicit def instance: SemigroupK[T] = new SemigroupK[T] {
    def combineK[A](x: T[A], y: T[A]): T[A] = combine(x, y)
  }
}

trait SemigroupNewtypeReq[Req[_]] {
  abstract final class Tag
  type T[X] = newtype.Newtype[X, Tag]

  def apply[X](x: X): T[X] = newtype(x)

  def combine[X](x: X, y: X)(implicit req: Req[X]): X

  implicit def instance[A](implicit req: Req[A]): Semigroup[T[A]] = new Semigroup[T[A]] {
    def combine(x: T[A], y: T[A]): T[A] = combine(x, y)
  }
}

object First extends SemigroupNewtype {
  def combine[X](x: X, y: X): X = x
}

object Last extends SemigroupNewtype {
  def combine[X](x: X, y: X): X = y
}

object Sum extends SemigroupNewtypeReq[Numeric] {
  def combine[X](x: X, y: X)(implicit req: Numeric[X]): X = x + y
}

object Prod extends SemigroupNewtypeReq[Numeric] {
  def combine[X](x: X, y: X)(implicit req: Numeric[X]): X = x * y
}

object Min extends SemigroupNewtypeReq[Order] {
  def combine[X](x: X, y: X)(implicit req: Order[X]): X = x min y
}

object Max extends SemigroupNewtypeReq[Order] {
  def combine[X](x: X, y: X)(implicit req: Order[X]): X = x max y
}

object semigroups {
  type First[X] = First.T[X]
  type Last[X] = Last.T[X]
  type Sum[X] = Sum.T[X]
  type Prod[X] = Prod.T[X]
  type Min[X] = Prod.T[X]
  type Max[X] = Prod.T[X]
}

