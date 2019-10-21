package ru.tinkoff.tschema.utils

import tofu.optics.tags.{TagApply, TaggerObj}
import tofu.optics.{PSubset, Subset}

object subsets {
  object some extends TaggerObj[PSubset] {
    implicit def someOption[A]: TagApply[PSubset, Option[A], A, this.type, Unit] = _ => someSubset
  }

  private def someSubset[A]: Subset[Option[A], A] = new Subset[Option[A], A] {
    override def narrow(s: Option[A]): Either[Option[A], A] = s match {
      case None => Left(s)
      case Some(value) => Right(value)
    }

    override def upcast(b: A): Option[A] = Some(b)
  }
}