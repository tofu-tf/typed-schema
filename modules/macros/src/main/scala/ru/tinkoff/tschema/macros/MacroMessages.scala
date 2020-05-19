package ru.tinkoff.tschema.macros
import scala.reflect.macros.blackbox

trait MacroMessages {
  val c: blackbox.Context

  def abort(s: String) = c.abort(c.enclosingPosition, s)
  def info(s: String)  = c.info(c.enclosingPosition, s, force = true)
}
