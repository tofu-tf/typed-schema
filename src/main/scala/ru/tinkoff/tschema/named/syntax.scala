package ru.tinkoff.tschema.named
import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.named
import shapeless.Coproduct
import shapeless.ops.coproduct.Align

object syntax {
  implicit class NamedServeOps[x](x: ⇒ x) {
    def route[T, In <: Coproduct, Out <: Coproduct, Out1 <: Coproduct]
    (impl: T)
    (implicit serve: named.Serve.Aux[x, In, Out],
     routable: Routable.Aux[In, T, Out1],
     align: Align[Out, Out1]): Route =
      serve.handle(in ⇒ routable.routeWith(in, impl))

    def servePrefix(implicit prefix: ServePrefix[x]): ServePrefix.KAux[x, prefix.Input, prefix.Key] = prefix
    def servePostfix(implicit postfix: ServePostfix[x]): ServePostfix.Aux[x, postfix.Output] = postfix
    def serveSingle(implicit single: ServeSingle[x]): ServeSingle.TAux[x, single.Input, single.Output, single.Key] = single
    def serve(implicit serve: Serve[x]): Serve.Aux[x, serve.Input, serve.Output] = serve
    def typeof: TypeContainer[x] = new TypeContainer[x]
  }

  class TypeContainer[x] private[syntax] {
    type Type = x
  }
}
