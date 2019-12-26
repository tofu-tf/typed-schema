package tschema.utils

import cats.Eval
import tofu.optics.{PUpdate, Update}

object updates {
  def eval[A]: Update[Eval[A], A] = (ev, f) => ev.map(f)
}
