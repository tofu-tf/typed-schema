package ru.tinkoff.tschema
import ru.tinkoff.tschema.common.Rename
import ru.tinkoff.tschema.typeDSL._
import shapeless.Witness

object syntax extends CommonSyntax {
  def prefix[s](witness: Witness.Aux[s])      = new Prefix[s]
  def queryFlag[s](witness: Witness.Aux[s])   = new QueryFlag[s]
  def tag[s](witness: Witness.Aux[s])         = new Tag[s]
  def key[s](witness: Witness.Aux[s])         = new Key[s]
  def group[s](witness: Witness.Aux[s])       = new Group[s]
  def tagPrefix[s](witness: Witness.Aux[s])   = prefix[s](witness) |> tag[s](witness)
  def keyPrefix[s](witness: Witness.Aux[s])   = prefix[s](witness) |> key[s](witness)
  def groupPrefix[s](witness: Witness.Aux[s]) = prefix[s](witness) |> group[s](witness)
  def operation[s](witness: Witness.Aux[s])   = keyPrefix[s](witness)

  def snake[name](name: Witness.Aux[name]): Rename[name, Rename.snakeCase] = new Rename
  def kebab[name](name: Witness.Aux[name]): Rename[name, Rename.kebabCase] = new Rename
  def lower[name](name: Witness.Aux[name]): Rename[name, Rename.lowerCase] = new Rename
  def upper[name](name: Witness.Aux[name]): Rename[name, Rename.upperCase] = new Rename

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
