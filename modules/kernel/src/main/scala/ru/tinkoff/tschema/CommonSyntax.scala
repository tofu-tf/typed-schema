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

  implicit class TypeApiOps[x <: DSLDef](x: => x) {
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

trait NamedSyntaxOps[s] {
  import syntax.TypeApiOps

  def prefix    = new Prefix[s]
  def queryFlag = new QueryFlag[s]
  def tag       = new Tag[s]
  def key       = new Key[s]
  def group     = new Group[s]

  def tagPrefix             = prefix |> tag
  def keyPrefix             = prefix |> key
  def groupPrefix           = prefix |> group
  def operation             = keyPrefix
  def allQuery: AllQuery[s] = new AllQuery[s]

  def capture[x]     = new Capture[s, x]
  def queryParam[x]  = new QueryParam[s, x]
  def queryParams[x] = new QueryParams[s, x]

  def header[x]    = new Header[s, x]
  def formField[x] = new FormField[s, x]
  def cookie[x]    = new Cookie[s, x]
  def body[x]      = new ReqBody[s, x]
}
