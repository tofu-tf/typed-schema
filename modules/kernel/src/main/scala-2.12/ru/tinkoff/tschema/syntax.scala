package ru.tinkoff.tschema
import ru.tinkoff.tschema.typeDSL._
import shapeless.Witness

object syntax {
  def prefix[s](witness: Witness.Aux[s])      = new Prefix[s]
  def queryFlag[s](witness: Witness.Aux[s])   = new QueryFlag[s]
  def tag[s](witness: Witness.Aux[s])         = new Tag[s]
  def key[s](witness: Witness.Aux[s])         = new Key[s]
  def group[s](witness: Witness.Aux[s])       = new Group[s]
  def deprecated                              = new Deprecated
  def tagPrefix[s](witness: Witness.Aux[s])   = prefix[s](witness) |> tag[s](witness)
  def keyPrefix[s](witness: Witness.Aux[s])   = prefix[s](witness) |> key[s](witness)
  def groupPrefix[s](witness: Witness.Aux[s]) = prefix[s](witness) |> group[s](witness)
  def operation[s](witness: Witness.Aux[s])   = keyPrefix[s](witness)

  def allQuery[name](s: Witness.Aux[name]): AllQuery[name] = new AllQuery[name]

  def capture[x] =
    new MkComplex(new Maker[x, Capture] {
      override def make[s]: Capture[s, x] = new Capture
    })

  def queryParam[x] =
    new MkComplex(new Maker[x, QueryParam] {
      override def make[s]: QueryParam[s, x] = new QueryParam
    })

  def queryParams[x] =
    new MkComplex(new Maker[x, QueryParams] {
      override def make[s]: QueryParams[s, x] = new QueryParams
    })

  def header[x] =
    new MkComplex(new Maker[x, Header] {
      override def make[s]: Header[s, x] = new Header
    })

  def formField[x] =
    new MkComplex(new Maker[x, FormField] {
      override def make[s]: FormField[s, x] = new FormField
    })

  def cookie[x] =
    new MkComplex(new Maker[x, Cookie] {
      override def make[s]: Cookie[s, x] = new Cookie
    })

  def body[x] =
    new MkComplex(new Maker[x, ReqBody] {
      override def make[s]: ReqBody[s, x] = new ReqBody
    })

  def reqBody[x] = new ReqBody[Witness.`'body`.T, x]

  abstract class Maker[x, T[_, _]] {
    def make[s]: T[s, x]
  }

  class MkComplex[x, T[_, _]](maker: Maker[x, T]) {
    def apply[s](witness: Witness.Aux[s]) = maker.make[s]
  }

  implicit class TypeApiOps[x <: DSLDef](x: => x) {
    def ~[y](y: => y): x <|> y                           = new <|>(x, y)
    def <|>[y](y: => y): x <|> y                         = new <|>(x, y)
    def <>[y](y: => y): x <|> y                          = new <|>(x, y)
    def :>[y](y: => y): x :> y                           = new :>
    def |>[y](y: => y): x :> y                           = new :>
    def &[y](y: => y): x :> y                            = new :>
    def apply[y](y: => y): x :> y                        = new :>
    def as[name](name: Witness.Aux[name]): x :> As[name] = new :>
  }

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

  def opGet     = key("get") |> new Get
  def opPost    = key("post") |> new Post
  def opPut     = key("put") |> new Put
  def opDelete  = key("delete") |> new Delete
  def opHead    = key("head") |> new Head
  def opOptions = key("options") |> new Options
  def opPatch   = key("patch") |> new Patch

  def complete[x]: Complete[x] = new Complete[x]
  def $$[x]: Complete[x]       = new Complete[x]

  def basicAuth[x] = new MkBasicAuth[x]

  class MkBasicAuth[x] {
    def apply[realm, name](realm: Witness.Aux[realm], name: Witness.Aux[name]): BasicAuth[realm, name, x] =
      new BasicAuth
  }

  def bearerAuth[x] = new MkBearerAuth[x]

  class MkBearerAuth[x] {
    def apply[realm, name](realm: Witness.Aux[realm], name: Witness.Aux[name]): BearerAuth[realm, name, x] =
      new BearerAuth
  }

  def apiKeyAuth[realm, Param <: CanHoldApiKey](realm: Witness.Aux[realm], param: Param): ApiKeyAuth[realm, Param] =
    new ApiKeyAuth
}
