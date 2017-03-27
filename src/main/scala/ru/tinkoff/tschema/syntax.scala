package ru.tinkoff.tschema

import ru.tinkoff.tschema.macros.ParamMaker
import ru.tinkoff.tschema.swagger.{Description, Tag}
import shapeless._
import typeDSL._

import scala.language.higherKinds

object syntax {
  def prefix[s](witness: Witness.Lt[s]) = new Prefix[s]
  def queryFlag[s](witness: Witness.Lt[s]) = new QueryFlag[s]
  def tag[s](witness: Witness.Lt[s]) = new Tag[s]
  def key[s](witness: Witness.Lt[s]) = new Key[s]
  def tagPrefix[s](witness: Witness.Lt[s]) = prefix[s](witness) :> tag[s](witness)
  def keyPrefix[s](witness: Witness.Lt[s]) = prefix[s](witness) :> key[s](witness)
  def operation[s](witness: Witness.Lt[s]) = keyPrefix[s](witness) :> descr.i18n[s](witness)

  object descr {
    def static[s](witness: Witness.Lt[s]) = new Description.Static[s]
    def resource[s](witness: Witness.Lt[s]) = new Description.Resource[s]
    def i18n[s](witness: Witness.Lt[s]) = new Description.I18n[s]
  }

  def capture[x] = new MkComplex(new Maker[x, Capture] {
    override def make[s]: Capture[s, x] = new Capture
  })

  def queryParam[x] = new MkComplex(new Maker[x, QueryParam] {
    override def make[s]: QueryParam[s, x] = new QueryParam
  })

  def queryParams[x] = new MkComplex(new Maker[x, QueryParams] {
    override def make[s]: QueryParams[s, x] = new QueryParams
  })

  def header[x] = new MkComplex(new Maker[x, Header] {
    override def make[s]: Header[s, x] = new Header
  })

  def formField[x] = new MkComplex(new Maker[x, FormField] {
    override def make[s]: FormField[s, x] = new FormField
  })

  def cookie[x] = new MkComplex(new Maker[x, Cookie] {
    override def make[s]: Cookie[s, x] = new Cookie
  })

  abstract class Maker[x, T[_, _]] {
    def make[s]: T[s, x]
  }

  class MkComplex[x, T[_, _]](maker: Maker[x, T]) {
    def apply[s](witness: Witness.Lt[s]) = maker.make[s]
  }

  implicit class TypeApiOps[x <: DSLAtom](x: ⇒ x) {
    def <|>[y](y: ⇒ y): x <|> y = new <|>(x, y)
    def :>[y](y: ⇒ y): x :> y = new :>
    def apply[y](y: ⇒ y): x :> y = new :>
  }

  object query extends ParamMaker[QueryParam]
  object path extends ParamMaker[Capture]
  object headers extends ParamMaker[Header]
  object form extends ParamMaker[FormField]
}
