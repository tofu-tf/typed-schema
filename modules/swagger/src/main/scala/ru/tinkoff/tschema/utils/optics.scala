package ru.tinkoff.tschema.utils

import cats.Eval
import tofu.optics.tags.{TagApply, TaggerObj}
import tofu.optics.{Contains, PSubset, Subset, Update}

object optics {
  object _some extends TaggerObj[PSubset] {
    implicit def someOption[A]: TagApply[PSubset, Option[A], A, this.type, Unit] = _ => someSubset
  }

  private def someSubset[A]: Subset[Option[A], A] = new Subset[Option[A], A] {
    override def narrow(s: Option[A]): Either[Option[A], A] = s match {
      case None        => Left(s)
      case Some(value) => Right(value)
    }

    override def upcast(b: A): Option[A]                    = Some(b)
  }

  def eval[A]: Update[Eval[A], A] = (ev, f) => ev.map(f)

  def mapKeyVals[K, V]: Update[Map[K, V], (K, V)] = _ map _

  def first[A, B]: Contains[(A, B), A] = new Contains[(A, B), A] {
    def set(s: (A, B), b: A): (A, B) = (b, s._2)
    def extract(s: (A, B)): A        = s._1
  }

  def second[A, B]: Contains[(A, B), B] = new Contains[(A, B), B] {
    def set(s: (A, B), b: B): (A, B) = (s._1, b)
    def extract(s: (A, B)): B        = s._2
  }
}
