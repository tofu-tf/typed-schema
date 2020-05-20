package ru.tinkoff.tschema
import ru.tinkoff.tschema.common.NameTrans
import ru.tinkoff.tschema.typeDSL._

object syntax extends CommonSyntax {
  def prefix[s <: Singleton](s: s)                            = new Prefix[s]
  def queryFlag[s <: Singleton](s: s)                         = new QueryFlag[s]
  def tag[s <: Singleton](s: s)                               = new Tag[s]
  def key[s <: Singleton](s: s)                               = new Key[s]
  def group[s <: Singleton](s: s)                             = new Group[s]
  def tagPrefix[s <: Singleton](s: s)                         = prefix[s](s) |> tag[s](s)
  def keyPrefix[s <: Singleton](s: s)                         = prefix[s](s) |> key[s](s)
  def groupPrefix[s <: Singleton](s: s)                       = prefix[s](s) |> group[s](s)
  def operation[s <: Singleton](s: s)                         = keyPrefix[s](s)
  def allQuery[name <: Singleton](name: name): AllQuery[name] = new AllQuery[name]

  def snake[name <: Singleton](name: name): NameTrans[name, NameTrans.snakeCase] = new NameTrans
  def kebab[name <: Singleton](name: name): NameTrans[name, NameTrans.kebabCase] = new NameTrans
  def lower[name <: Singleton](name: name): NameTrans[name, NameTrans.lowerCase] = new NameTrans
  def upper[name <: Singleton](name: name): NameTrans[name, NameTrans.upperCase] = new NameTrans

  def renamed[name <: Singleton, as <: Singleton](name: name, as: as): Renamed[name, as] = new Renamed

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

  def reqBody[x] = new ReqBody["body", x]

  abstract class Maker[x, T[_, _]] {
    def make[s]: T[s, x]
  }

  class MkComplex[x, T[_, _]](maker: Maker[x, T]) {
    def apply[s <: Singleton](s: s) = maker.make[s]
  }

  def basicAuth[x] = new MkBasicAuth[x]

  class MkBasicAuth[x] {
    def apply[realm <: Singleton, name <: Singleton](realm: realm, name: name): BasicAuth[realm, name, x] =
      new BasicAuth
  }

  def bearerAuth[x] = new MkBearerAuth[x]

  class MkBearerAuth[x] {
    def apply[realm <: Singleton, name <: Singleton](realm: realm, name: name): BearerAuth[realm, name, x] =
      new BearerAuth
  }

  def apiKeyAuth[realm <: Singleton, Param <: CanHoldApiKey](realm: realm, param: Param): ApiKeyAuth[realm, Param] =
    new ApiKeyAuth
}
