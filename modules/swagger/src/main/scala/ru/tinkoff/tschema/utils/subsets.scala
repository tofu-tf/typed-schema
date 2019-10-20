package ru.tinkoff.tschema.utils

import tofu.optics.Subset

object subsets {
  def some[A]: Subset[Option[A], A] = new Subset[Option[A], A] {
    override def narrow(s: Option[A]): Either[Option[A], A] = s match {
      case None => Left(s)
      case Some(value) => Right(value)
    }

    override def upcast(b: A): Option[A] = Some(b)
  }
}