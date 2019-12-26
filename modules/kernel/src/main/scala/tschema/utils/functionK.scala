package tschema.utils

import cats.arrow.FunctionK

final class MkFunctionK[F[_]](val dummy: Boolean = true) extends AnyVal {
  type T

  def apply[G[_]](f: F[T] => G[T]): FunctionK[F, G] = new FunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa.asInstanceOf[F[T]]).asInstanceOf[G[A]]
  }
}

object functionK {
  def apply[F[_]]: MkFunctionK[F] = new MkFunctionK
}
