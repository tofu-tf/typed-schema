package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.macros.NamedImpl
import ru.tinkoff.tschema.named
import shapeless.{Coproduct, HList, HNil, Witness}
import shapeless.ops.coproduct.Align

object syntax {
  implicit class NamedServeOps[x](x: ⇒ x) {
    def route[T, In <: Coproduct, Out <: Coproduct, Res <: Coproduct]
    (impl: T)
    (implicit serve: Serve.Aux[x, HNil, In, Out],
     namedImpl: NamedImpl.Aux[T, In, Res],
     routable: RoutableUnion[Res, Out]): Route =
      serve(in ⇒ routable.route(namedImpl.produce(in, impl)), Provide.empty)

    def servePrefix(implicit prefix: ServePrefix[x, HNil]): ServePrefix.KAux[x, HNil, prefix.Input, prefix.Key] = prefix
    def servePrefixP[P <: HList](implicit prefix: ServePrefix[x, P]): ServePrefix.KAux[x, P, prefix.Input, prefix.Key] = prefix
    def servePostfix(implicit postfix: ServePostfix[x]): ServePostfix.Aux[x, postfix.Output] = postfix
    def serveSingle(implicit single: ServeSingle[x, HNil]): ServeSingle.TAux[x, HNil, single.Input, single.Output, single.Key] = single
    def serveSingleP[P <: HList](implicit single: ServeSingle[x, P]): ServeSingle.TAux[x, P, single.Input, single.Output, single.Key] = single
    def serveMiddle[key](key: Witness.Lt[key])(implicit middle: ServeMiddle[x, HNil, key]): ServeMiddle[x, HNil, key] = middle
    def serveMiddleP[key, P <: HList](key: Witness.Lt[key])(implicit middle: ServeMiddle[x, P, key]): ServeMiddle[x, P, key] = middle
    def serve(implicit serve: Serve[x, HNil]): Serve.Aux[x, HNil, serve.Input, serve.Output] = serve
    def serveP[P <: HList](implicit serve: Serve[x, P]): Serve.Aux[x, P, serve.Input, serve.Output] = serve
    def typeof: TypeContainer[x] = new TypeContainer[x]
  }

  class TypeContainer[x] private[syntax] {
    type Type = x
  }
}
