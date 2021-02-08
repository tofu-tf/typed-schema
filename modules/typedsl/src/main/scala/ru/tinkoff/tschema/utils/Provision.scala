package ru.tinkoff.tschema.utils

trait Provision[F[_], A] {
  def provide(): F[Option[A]]
}
