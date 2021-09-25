package ru.tinkoff.tschema

import ru.tinkoff.tschema.typeDSL._

class CommonSyntax { self: syntax.type =>
  def deprecated = new Deprecated

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

  implicit class TypeApiOps[x <: DSLDef](x: => x)     {
    def ~[y](y: => y): x <|> y                           = new <|>(x, y)
    def <|>[y](y: => y): x <|> y                         = new <|>(x, y)
    def <>[y](y: => y): x <|> y                          = new <|>(x, y)
    def :>[y](y: => y): x :> y                           = new :>
    def |>[y](y: => y): x :> y                           = new :>
    def &[y](y: => y): x :> y                            = new :>
    def apply[y](y: => y): x :> y                        = new :>
    def as[name <: Singleton](name: name): x :> As[name] = new :>
  }

  implicit class ResultMaker[x <: DSLMethod](x: => x) {
    def apply[A]: x :> Complete[A] = x :> new Complete
    def ! : x                      = x
  }
}

trait NamedSyntaxOps[trans, orig] {
  def prefix: Prefix[trans]       = new Prefix
  def queryFlag: QueryFlag[trans] = new QueryFlag

  def tagPrefix: Prefix[trans] :> Tag[orig]     = new :>
  def keyPrefix: Prefix[trans] :> Key[orig]     = new :>
  def groupPrefix: Prefix[trans] :> Group[orig] = new :>
  def operation: Prefix[trans] :> Key[orig]     = new :>
  def allQuery: AllQuery[trans]                 = new AllQuery

  def capture[x]     = new Capture[trans, x]
  def queryParam[x]  = new QueryParam[trans, x]
  def queryParams[x] = new QueryParams[trans, x]

  def header[x]         = new Header[trans, x]
  def formField[x]      = new FormField[trans, x]
  def multipartField[x] = new MultipartField[trans, x]
  def cookie[x]         = new Cookie[trans, x]
  def body[x]           = new ReqBody[trans, x]
}

class Renamed[orig, param] extends NamedSyntaxOps[orig, param]
