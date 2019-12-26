package tschema
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.server.{Directives, Rejection, Route}

package object akkaHttp {
  def rejectionHandler: PartialFunction[Rejection, Route] = {
    case NotFoundPathRejection(name) =>
      Directives.complete(BadRequest, s"could not find path parameter $name")
    case MalformedPathRejection(name, formatError) =>
      Directives.complete(BadRequest, s"could not parse path parameter $name : $formatError")
  }
}

package akkaHttp {
  final case class NotFoundPathRejection(name: String)                       extends Rejection
  final case class MalformedPathRejection(name: String, formatError: String) extends Rejection
}
