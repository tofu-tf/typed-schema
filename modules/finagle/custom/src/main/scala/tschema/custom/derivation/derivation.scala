package tschema.custom
package derivation
import com.twitter.finagle.http.Response
import org.manatki.derevo.Derivation
import tschema.ResponseStatus

object responseStatus extends Derivation[ResponseStatus] {
  def apply[T](status: Int): ResponseStatus[T] = ResponseStatus(status)
}

object jsonError extends Derivation[ErrorResponse] {
  def apply[T](status: Int)(implicit json: AsResponse.Json[T]): ErrorResponse[T] =
    new ErrorResponse[T](status) {
      def apply(a: T): Response = json(a).statusCode(status)
    }
}

object plainError extends Derivation[ErrorResponse]{
  def apply[T](status: Int)(implicit plain: AsResponse.Plain[T]): ErrorResponse[T] =
    new ErrorResponse[T](status) {
      def apply(a: T): Response = plain(a).statusCode(status)
    }
}
