package ru.tinkoff.tschema

import ru.tinkoff.tschema.common.HasReq
import ru.tinkoff.tschema.macros.ParamMaker
import ru.tinkoff.tschema.swagger.{Description, Tag}
import shapeless._
import typeDSL._

import scala.language.higherKinds

object syntax {
  def prefix[s <: Symbol](witness: Witness.Lt[s]) = new Prefix[s]
  def queryFlag[s <: Symbol](witness: Witness.Lt[s]) = new QueryFlag[s]
  def tag[s <: Symbol](witness: Witness.Lt[s]) = new Tag[s]
  def key[s <: Symbol](witness: Witness.Lt[s]) = new Key[s]
  def tagPrefix[s <: Symbol](witness: Witness.Lt[s]) = prefix[s](witness) |> tag[s](witness)
  def keyPrefix[s <: Symbol](witness: Witness.Lt[s]) = prefix[s](witness) |> key[s](witness)
  def operation[s <: Symbol](witness: Witness.Lt[s]) = keyPrefix[s](witness) |> descr.i18n[s](witness)
  def keyi18n[s <: Symbol](witness: Witness.Lt[s]) = key[s](witness) |> descr.i18n[s](witness)

  def reqBody[x] = new ReqBody[x]

  object descr {
    def static[s](witness: Witness.Lt[s]) = new Description.Static[s]
    def resource[s](witness: Witness.Lt[s]) = new Description.Resource[s]
    def i18n[s](witness: Witness.Lt[s]) = new Description.I18n[s]
  }

  def capture[x] = new MkComplex(new Maker[x, Capture] {
    override def make[s <: Symbol]: Capture[s, x] = new Capture
  })

  def queryParam[x] = new MkComplex(new Maker[x, QueryParam] {
    override def make[s <: Symbol]: QueryParam[s, x] = new QueryParam
  })

  def queryParams[x] = new MkComplex(new Maker[x, QueryParams] {
    override def make[s <: Symbol]: QueryParams[s, x] = new QueryParams
  })

  def header[x] = new MkComplex(new Maker[x, Header] {
    override def make[s <: Symbol]: Header[s, x] = new Header
  })

  def formField[x] = new MkComplex(new Maker[x, FormField] {
    override def make[s <: Symbol]: FormField[s, x] = new FormField
  })

  def cookie[x] = new MkComplex(new Maker[x, Cookie] {
    override def make[s <: Symbol]: Cookie[s, x] = new Cookie
  })

  abstract class Maker[x, T[_, _]] {
    def make[s <: Symbol]: T[s, x]
  }

  class MkComplex[x, T[_, _]](maker: Maker[x, T]) {
    def apply[s <: Symbol](witness: Witness.Lt[s]) = maker.make[s]
  }

  class MkTransform[a, b] {
    def apply[t, u <: Symbol, s <: Symbol](t: t)(wu: Witness.Lt[u], ws: Witness.Lt[s]) = new Transform[u, s, t, a, b]
  }

  class MkTransformReq[a, b] {
    def apply[t <: HasReq, u <: Symbol, s <: Symbol](t: t)(wu: Witness.Lt[u], ws: Witness.Lt[s]) = new Transform[u, s, t, a, b]
  }

  implicit class TypeApiOps[x <: DSLDef](x: => x) {
    def ~[y](y: => y): x <|> y = new <|>(x, y)
    def <|>[y](y: => y): x <|> y = new <|>(x, y)
    def <>[y](y: => y): x <|> y = new <|>(x, y)
    def :>[y](y: => y): x :> y = new :>
    def |>[y](y: => y): x :> y = new :>
    def &[y](y: => y): x :> y = new :>
    def apply[y](y: => y): x :> y = new :>
  }

  object query extends ParamMaker[QueryParam]
  object path extends ParamMaker[Capture]
  object headers extends ParamMaker[Header]
  object form extends ParamMaker[FormField]

  sealed class ResultMaker[F[_]](x: => F[Nothing]){
    def apply[x]: F[x] = x.asInstanceOf[F[x]]
  }

  object get extends ResultMaker(new Get)
  object post extends ResultMaker(new Post)
  object put extends ResultMaker(new Put)
  object delete extends ResultMaker(new Delete)
  object head extends ResultMaker(new Head)
  object options extends ResultMaker(new Options)
  object patch extends ResultMaker(new Patch)
  object complete extends ResultMaker(new Complete)
}
