package tschema.finagle.util

final case class Unapply[A, B](f: A => Option[B]) {
  def unapply(a: A): Option[B] = f(a)
}

object Unapply {
  def apply[A, B](f: PartialFunction[A, B]): Unapply[A, B] = Unapply(f.lift)
}
