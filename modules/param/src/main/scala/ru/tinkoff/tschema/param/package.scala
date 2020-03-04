package ru.tinkoff.tschema
import tschema.param.ParamSource.All

package object param {
  type ParamSource = tschema.param.ParamSource
  val ParamSource: tschema.param.ParamSource.type = tschema.param.ParamSource

  type Param[+S >: All <: ParamSource, A] = tschema.param.Param[S, A]
  val Param: tschema.param.Param.type = tschema.param.Param

  type SingleParam[+S >: All <: ParamSource, A] = tschema.param.SingleParam[S, A]
  val SingleParam: tschema.param.SingleParam.type = tschema.param.SingleParam

  type SingleParamReq[+S >: All <: ParamSource, A] = tschema.param.SingleParamReq[S, A]

  type MultiParam[+S >: All <: ParamSource, A] = tschema.param.MultiParam[S, A]
  val MultiParam: tschema.param.MultiParam.type = tschema.param.MultiParam

  type MultiParamReq[+S >: All <: ParamSource, A] = tschema.param.MultiParamReq[S, A]

  type HttpParam[A] = tschema.param.HttpParam[A]
  val HttpParam: tschema.param.HttpParam.type = tschema.param.HttpParam

  type HttpSingleParam[A] = tschema.param.HttpSingleParam[A]
  val HttpSingleParam: tschema.param.HttpSingleParam.type = tschema.param.HttpSingleParam

  type HttpSingleParamReq[A] = tschema.param.HttpSingleParamReq[A]
  type HttpMultiParam[A]     = tschema.param.HttpMultiParam[A]

  type ParamError       = tschema.param.ParamError
  type SingleParamError = tschema.param.SingleParamError
  val MissingParamError: tschema.param.MissingParamError.type = tschema.param.MissingParamError

  type ParseParamError = tschema.param.ParseParamError
  val ParseParamError: tschema.param.ParseParamError.type = tschema.param.ParseParamError

  type MultiParamError = tschema.param.MultiParamError
  val MultiParamError: tschema.param.MultiParamError.type = tschema.param.MultiParamError
}
