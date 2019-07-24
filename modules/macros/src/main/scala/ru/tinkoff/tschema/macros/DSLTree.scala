package ru.tinkoff.tschema.macros

import cats.kernel.Monoid
import Vector.{empty => vnil}

sealed trait DSLTree[T]
case class DSLBranch[T](pref: Vector[T], children: Vector[DSLTree[T]]) extends DSLTree[T]
case class DSLLeaf[T](res: T, key: String)                             extends DSLTree[T]

final case class PrefixInfo[T](key: Option[String], groups: Vector[String], prefix: Vector[T])

object PrefixInfo {
  implicit def keyInfoMonoid[T]: Monoid[PrefixInfo[T]] = new Monoid[PrefixInfo[T]] {
    def empty = PrefixInfo(None, Vector(), Vector())
    def combine(x: PrefixInfo[T], y: PrefixInfo[T]) =
      PrefixInfo(y.key orElse x.key, x.groups ++ y.groups, x.prefix ++ y.prefix)
  }
}
