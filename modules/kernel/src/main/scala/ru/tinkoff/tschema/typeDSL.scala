package ru.tinkoff.tschema

object typeDSL {
  trait DSLDef
  trait DSLAtom   extends DSLDef
  trait DSLMethod extends DSLAtom

  trait CanHoldApiKey
  trait DSLAtomAuth extends DSLAtom

  /**
    * Any path component that is subtype of Meta will be ignored
    * by `Serve` but could support additional information like swagger tags or descriptions
    */
  trait Meta extends DSLAtom

  /**
    * naming symbol of single route in complex route
    */
  class Key[key] extends DSLAtom

  /**
    * indicates result of element of type `x`
    * does not check HTTP method
    */
  final class Complete[x] extends DSLDef

  /**
    * indicates result of element of type `x`
    * via POST HTTP method
    */
  final class Post extends DSLMethod

  /**
    * indicates result of element of type `x`
    * via GET HTTP method
    */
  final class Get extends DSLMethod

  /**
    * indicates result of element of type `x`
    * via PUT HTTP method
    */
  final class Put extends DSLMethod

  /**
    * indicates result of element of type `x`
    * via DELETE HTTP method
    */
  final class Delete extends DSLMethod

  /**
    * indicates result of element of type `x`
    * via HEAD HTTP method
    */
  final class Head extends DSLMethod

  /**
    * indicates result of element of type `x`
    * via OPTIONS HTTP method
    */
  final class Options extends DSLMethod

  /**
    * indicates result of element of type `x`
    * via PATCH HTTP method
    */
  final class Patch extends DSLAtom

  /**
    * Indicated single path prefix
    * Could be replaced by it's parameter
    *
    * @tparam pref singleton string
    */
  final class Prefix[pref] extends DSLAtom

  /**
    * captures param from query
    *
    * @tparam name name of param
    * @tparam x    type of param, should have instance of `FromQueryParam`
    */
  final class QueryParam[name, x] extends DSLAtom with CanHoldApiKey

  /**
    * captures param from path element
    *
    * @tparam name name of param, have no effect to routing
    * @tparam x    type of param, should have instance of `FromPathParam`
    */
  final class Capture[name, x] extends DSLAtom

  /**
    * captures param list from query
    *
    * @tparam name name of param
    * @tparam x    type of param, should have instance of `FromQueryParam`
    */
  final class QueryParams[name, x] extends DSLAtom

  /**
    * captures fact of provision of param in query
    *
    * @tparam name name of param
    */
  final class QueryFlag[name] extends DSLAtom

  final class AllQuery[name, x] extends DSLAtom

  /**
    * captures request body and unmarshalls in to requested type
    *
    * @tparam x type of body, should have `FromRequestUnmarshaller` instance
    */
  final class ReqBody[name, x] extends DSLAtom

  /**
    * captures header value
    *
    * @tparam name header name
    * @tparam x    parameter type, should have `FromHeader` instance
    */
  final class Header[name, x] extends DSLAtom with CanHoldApiKey

  /**
    * captures field value from form data
    *
    * @tparam name field name
    * @tparam x    parameter type, should have `FromFormParam` instance
    */
  final class FormField[name, x] extends DSLAtom

  /**
    * captures field value from Cookie
    */
  final class Cookie[name, x] extends DSLAtom with CanHoldApiKey

  /**
    * defines basic authentication scheme
    */
  final class BasicAuth[realm, name, T] extends DSLAtomAuth

  /**
    * defines bearer authentication scheme
    */
  final class BearerAuth[realm, name, T] extends DSLAtomAuth

  /**
    * defines api key authentication scheme
    * @tparam Param on of: Header, Cookie or Query
    */
  final class ApiKeyAuth[realm, Param <: CanHoldApiKey] extends DSLAtomAuth

  /**
    * transforms directive to rename provided parameter with given name
    */
  final class As[directive, name] extends DSLAtom

  /**
    * captures multiple fields from place
    *
    * @tparam place Header, FormField, Cookie or QueryParam
    * @tparam x     record type - only simple types, with modifiers like Option or List accepted
    */
  final class Record[place[_, _], x] extends DSLAtom

  /**
    * transforms existing parameter
    * introducing new parameter
    * using known transformation
    *
    * @tparam source
    */
  final class Transform[source, name, t, a, b] extends DSLAtom

  /**
    * transforms existing parameter
    * introducing new parameter
    * using known transformation
    *
    * @tparam source
    */
  final class TransformReq[source, name, t, a, b, req] extends DSLAtom

  /**
    * concatenates pair of paths into complete path
    *
    * @tparam path    prefix - always simple path without disjunctions
    * @tparam postfix postfix
    */
  final class :>[path, postfix] extends DSLDef

  /**
    * disjunction operator
    * can be used both for defining API type and for joining different handlers
    * resulting type is effectively `Either[left input, right input] => Either[left output, right output]`
    */
  final case class <|>[left, right](left: left, right: right) extends DSLDef {
    override def toString = s"$left <|> $right"
  }
}
