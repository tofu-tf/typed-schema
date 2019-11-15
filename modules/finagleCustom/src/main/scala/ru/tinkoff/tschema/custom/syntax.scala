package ru.tinkoff.tschema.custom
import ru.tinkoff.tschema.syntax.{Maker, MkComplex}
import ru.tinkoff.tschema.typeDSL._

object syntax {
  def json[A]: Complete[JsonResult[A]] = new Complete

  def plain[A]: Complete[PlainResult[A]] = new Complete

  def err[E, A]: Complete[ErrorResult[E, A]] = new Complete

  def jsonErr[E, A]: Complete[ErrorResult[E, JsonResult[A]]] = new Complete

  def jsonBody[x] =
    new MkComplex(new Maker[x, JsonBody] {
      override def make[name]: JsonBody[name, x] = new JsonBody
    })
}
