package ru.tinkoff.tschema

object typeDSL {
  trait DSLDef
  trait DSLAtom   extends DSLDef
  trait DSLMethod extends DSLAtom

  trait CanHoldApiKey
  trait DSLAtomAuth extends DSLAtom

  /** Any path component that is subtype of Meta will be ignored by `Serve` but could support additional information
    * like swagger tags or descriptions
    */
  trait Meta extends DSLAtom

  /** tagging symbol for route
    */
  class Tag[tag] extends Meta

  /** mark operation as deprecated
    */
  class Deprecated extends Meta

  /** naming symbol of single route in complex route
    */
  class Key[key] extends DSLAtom

  object Key {
    private val anyKey: Key[Any] = new Key
    def of[key]: Key[key]        = anyKey.asInstanceOf[Key[key]]
  }

  /** naming intermediate group of methods
    */
  class Group[key] extends DSLAtom

  object Group {
    private val anyGroup: Group[Any] = new Group
    def of[key]: Group[key]          = anyGroup.asInstanceOf[Group[key]]
  }

  /** return query params as Map[String, String]
    */
  final class AllQuery[name] extends DSLAtom

  /** indicates result of element of type `x` does not check HTTP method
    */
  final class Complete[x] extends DSLDef

  /** indicates result of element of type `x` via POST HTTP method
    */
  final class Post extends DSLMethod

  /** indicates result of element of type `x` via GET HTTP method
    */
  final class Get extends DSLMethod

  /** indicates result of element of type `x` via PUT HTTP method
    */
  final class Put extends DSLMethod

  /** indicates result of element of type `x` via DELETE HTTP method
    */
  final class Delete extends DSLMethod

  /** indicates result of element of type `x` via HEAD HTTP method
    */
  final class Head extends DSLMethod

  /** indicates result of element of type `x` via OPTIONS HTTP method
    */
  final class Options extends DSLMethod

  /** indicates result of element of type `x` via PATCH HTTP method
    */
  final class Patch extends DSLMethod

  /** Indicated single path prefix Could be replaced by it's parameter
    *
    * @tparam pref
    *   singleton string
    */
  final class Prefix[pref] extends DSLAtom

  /** Indicates end of path
    */
  final class End extends DSLAtom

  /** captures param from query
    *
    * @tparam name
    *   name of param
    * @tparam x
    *   type of param, should have instance of `FromQueryParam`
    */
  final class QueryParamAs[name, p, x] extends DSLAtom with CanHoldApiKey
  type QueryParam[name, x] = QueryParamAs[name, name, x]

  /** captures param from path element
    *
    * @tparam name
    *   name of param, have no effect to routing
    * @tparam x
    *   type of param, should have instance of `FromPathParam`
    */
  final class CaptureAs[name, p, x] extends DSLAtom
  type Capture[name, x] = CaptureAs[name, name, x]

  /** captures param list from query
    *
    * @tparam name
    *   name of param
    * @tparam x
    *   type of param, should have instance of `FromQueryParam`
    */
  final class QueryParamsAs[name, p, x] extends DSLAtom
  type QueryParams[name, x] = QueryParamsAs[name, name, x]

  /** captures fact of provision of param in query
    *
    * @tparam name
    *   name of param
    */
  final class QueryFlagAs[name, p] extends DSLAtom
  type QueryFlag[name] = QueryFlagAs[name, name]

  /** captures request body and unmarshalls in to requested type
    *
    * @tparam x
    *   type of body, should have `FromRequestUnmarshaller` instance
    */
  final class ReqBodyAs[name, p, x] extends DSLAtom
  type ReqBody[name, x] = ReqBodyAs[name, name, x]

  /** captures header value
    *
    * @tparam name
    *   header name
    * @tparam x
    *   parameter type, should have `FromHeader` instance
    */
  final class HeaderAs[name, p, x] extends DSLAtom with CanHoldApiKey
  type Header[name, x] = HeaderAs[name, name, x]

  /** captures field value from form data
    *
    * @tparam name
    *   field name
    * @tparam x
    *   parameter type, should have `FromFormParam` instance
    */
  final class FormFieldAs[name, p, x] extends DSLAtom
  type FormField[name, x] = FormFieldAs[name, name, x]

  /** captures field value from multipart field data
    *
    * @tparam name
    *   field name
    * @tparam x
    *   parameter type, should have `FromMultipartParam` instance
    */
  final class MultipartFieldAs[name, p, x] extends DSLAtom
  type MultipartField[name, x] = MultipartFieldAs[name, name, x]

  /** captures field value from Cookie
    */
  final class CookieAs[name, p, x] extends DSLAtom with CanHoldApiKey
  type Cookie[name, x] = CookieAs[name, name, x]

  /** defines basic authentication scheme
    */
  final class BasicAuth[realm, name, T] extends DSLAtomAuth

  /** defines bearer authentication scheme
    */
  final class BearerAuth[realm, name, T] extends DSLAtomAuth

  /** defines oauth2 authentication
    */
  final class OAuth2Auth[+T, R, conf, name] extends DSLAtomAuth

  /** defines api key authentication scheme
    * @tparam Param
    *   on of: Header, Cookie or Query
    */
  final class ApiKeyAuth[realm, Param <: CanHoldApiKey] extends DSLAtomAuth

  /** transforms directive to rename provided parameter with given name
    */
  final class As[name] extends DSLAtom

  /** concatenates pair of paths into complete path
    *
    * @tparam path
    *   prefix - always simple path without disjunctions
    * @tparam postfix
    *   postfix
    */
  final class :>[path, postfix] extends DSLDef

  /** disjunction operator can be used both for defining API type and for joining different handlers resulting type is
    * effectively `Either[left input, right input] => Either[left output, right output]`
    */
  final case class <|>[left, right](left: left, right: right) extends DSLDef {
    override def toString = s"$left <|> $right"
  }
}
