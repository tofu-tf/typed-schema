package ru.tinkoff.tschema

import ru.tinkoff.tschema.common.HasReq
import ru.tinkoff.tschema.macros.ParamMaker
import ru.tinkoff.tschema.swagger.Tag
import shapeless._
import typeDSL._

import scala.language.higherKinds

object syntax {
  def prefix[s](witness: Witness.Lt[s])              = new Prefix[s]
  def queryFlag[s <: Symbol](witness: Witness.Lt[s]) = new QueryFlag[s]
  def allQuery[s <: Symbol](witness: Witness.Lt[s])  = new AllQuery[s]
  def tag[s <: Symbol](witness: Witness.Lt[s])       = new Tag[s]
  def key[s <: Symbol](witness: Witness.Lt[s])       = new Key[s]
  def tagPrefix[s <: Symbol](witness: Witness.Lt[s]) = prefix[s](witness) |> tag[s](witness)
  def keyPrefix[s <: Symbol](witness: Witness.Lt[s]) = prefix[s](witness) |> key[s](witness)
  def operation[s <: Symbol](witness: Witness.Lt[s]) = keyPrefix[s](witness)

  def capture[x] =
    new MkComplex(new Maker[x, Capture] {
      override def make[s <: Symbol]: Capture[s, x] = new Capture
    })

  def queryParam[x] =
    new MkComplex(new Maker[x, QueryParam] {
      override def make[s <: Symbol]: QueryParam[s, x] = new QueryParam
    })

  def queryParams[x] =
    new MkComplex(new Maker[x, QueryParams] {
      override def make[s <: Symbol]: QueryParams[s, x] = new QueryParams
    })

  def header[x] =
    new MkComplex(new Maker[x, Header] {
      override def make[s <: Symbol]: Header[s, x] = new Header
    })

  def formField[x] =
    new MkComplex(new Maker[x, FormField] {
      override def make[s <: Symbol]: FormField[s, x] = new FormField
    })

  def cookie[x] =
    new MkComplex(new Maker[x, Cookie] {
      override def make[s <: Symbol]: Cookie[s, x] = new Cookie
    })

  def body[x] =
    new MkComplex(new Maker[x, ReqBody] {
      override def make[s <: Symbol]: ReqBody[s, x] = new ReqBody
    })

  def reqBody[x] = new ReqBody[Witness.`'body`.T, x]

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
    def ~[y](y: => y): x <|> y                                  = new <|>(x, y)
    def <|>[y](y: => y): x <|> y                                = new <|>(x, y)
    def <>[y](y: => y): x <|> y                                 = new <|>(x, y)
    def :>[y](y: => y): x :> y                                  = new :>
    def |>[y](y: => y): x :> y                                  = new :>
    def &[y](y: => y): x :> y                                   = new :>
    def apply[y](y: => y): x :> y                               = new :>
    def as[name <: Symbol](name: Witness.Lt[name]): As[x, name] = new As
  }

  object query   extends ParamMaker[QueryParam]
  object path    extends ParamMaker[Capture]
  object headers extends ParamMaker[Header]
  object form    extends ParamMaker[FormField]

  implicit class ResultMaker[x <: DSLMethod](x: => x) {
    def apply[A]: x :> Complete[A] = x :> new Complete
    def ! : x                      = x
  }

  def get: Get         = new Get
  def post: Post       = new Post
  def put: Put         = new Put
  def delete: Delete   = new Delete
  def head: Head       = new Head
  def options: Options = new Options
  def patch: Patch     = new Patch

  def complete[x]: Complete[x] = new Complete[x]
  def $$[x]: Complete[x]       = new Complete[x]

  def basicAuth[x] = new MkBasicAuth[x]

  class MkBasicAuth[x] {
    def apply[realm, name <: Symbol](realm: Witness.Lt[realm], name: Witness.Lt[name]): BasicAuth[realm, name, x] =
      new BasicAuth
  }

  def bearerAuth[x] = new MkBearerAuth[x]

  class MkBearerAuth[x] {
    def apply[realm, name <: Symbol](realm: Witness.Lt[realm], name: Witness.Lt[name]): BearerAuth[realm, name, x] =
      new BearerAuth
  }

  def apiKeyAuth[realm, Param <: CanHoldApiKey](realm: Witness.Lt[realm], param: Param): ApiKeyAuth[realm, Param] =
    new ApiKeyAuth
}
