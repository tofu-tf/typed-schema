package ru.tinkoff.tschema.serve
import akka.http.scaladsl.server.Route

object syntax {
  implicit class SchemeServeOps[x](x: ⇒ x) {
    def serve[S, In, Out](servable: S)
                         (implicit serve: Serve[x, In, Out],
                          convert: ToServable[S, In, Out]): Route =
      serve.handle(servable.route)
  }
}
