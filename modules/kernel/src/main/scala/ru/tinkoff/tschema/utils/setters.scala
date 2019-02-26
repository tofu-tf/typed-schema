package ru.tinkoff.tschema.utils
import cats.Eval
import monocle.{PSetter, PTraversal}
import cats.instances.map._
import cats.instances.vector._

object setters {
  def map[K, A] = PSetter.fromFunctor[Map[K, ?], A, A]
  def eval[A] = PSetter.fromFunctor[Eval, A, A]
  def vector[A] = PTraversal.fromTraverse[Vector, A, A]
}
