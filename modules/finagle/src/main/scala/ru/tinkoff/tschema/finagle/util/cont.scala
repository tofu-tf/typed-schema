package ru.tinkoff.tschema.finagle.util
import cats.Eval
import cats.Eval.{defer, later}

object cont {

  /** using this instead of `cats.data.ContT` to drop `Defer` need
    * stack unsafe !!! */
  def traverseCont[F[_], X, A, B](xs: List[A])(f: (A, B => F[X]) => F[X])(k: List[B] => F[X]): F[X] = {
    def go(acc: List[B], xs1: List[A]): F[X] =
      xs1 match {
        case Nil      => k(acc.reverse)
        case x :: xs2 => f(x, b => go(b :: acc, xs2))
      }
    go(Nil, xs)
  }
}
