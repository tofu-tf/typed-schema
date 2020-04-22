package ru.tinkoff.tschema.custom

import ru.tinkoff.tschema.typeDSL._
import ru.tinkoff.tschema.syntax.{Maker, MkComplex}

object syntax {
  def json[A]: Complete[JsonResult[A]] = new Complete

  def plain[A]: Complete[PlainResult[A]] = new Complete

  def bin[CT, A]: Complete[BinResult[CT, A]] = new Complete

  def err[E, A]: Complete[ExceptResult[E, A]] = new Complete

  def jsonOpt[A]: Complete[ExceptResult[None.type, JsonResult[A]]] = new Complete

  def plainOpt[A]: Complete[ExceptResult[None.type, PlainResult[A]]] = new Complete

  def binOpt[CT, A]: Complete[ExceptResult[None.type, BinResult[CT, A]]] = new Complete

  def jsonErr[E, A]: Complete[ExceptResult[E, JsonResult[A]]] = new Complete

  def plainErr[E, A]: Complete[ExceptResult[E, PlainResult[A]]] = new Complete

  def binErr[E, CT, A]: Complete[ExceptResult[E, BinResult[CT, A]]] = new Complete

  def jsonBody[x] =
    new MkComplex(new Maker[x, JsonBody] {
      override def make[name]: JsonBody[name, x] = new JsonBody
    })

  def binBody = new MkComplex(new Maker[Array[Byte], BinBody] {
    override def make[name]: BinBody[name, Array[Byte]] = new BinBody
  })
}
