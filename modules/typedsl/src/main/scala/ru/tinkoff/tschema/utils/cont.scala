package ru.tinkoff.tschema.utils
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._

object cont {

  /** traversing continuations, heavily relying on stack-safety of F */
  def traverseCont[A, B, C, F[_]: Monad](items: List[A])(cont: (A, B => F[C]) => F[C])(k: List[B] => F[C]): F[C] = {
    def go(as: List[A], agg: List[B]): F[C] = as match {
      case Nil       => k(agg.reverse)
      case a :: rest => ().pure[F].flatMap(_ => cont(a, b => go(rest, b :: agg)))
    }
    go(items, Nil)
  }
}
