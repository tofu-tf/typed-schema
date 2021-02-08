package ru.tinkoff.tschema.finagle
import cats.{Applicative, Functor}
import com.twitter.finagle.http.Status
import ru.tinkoff.tschema.ResponseStatus
import ru.tinkoff.tschema.finagle.util.message
import ru.tinkoff.tschema.swagger.{SwaggerContent, SwaggerTypeable}

class StringCompleting[A](read: A => String, status: Int = 200) {
  implicit val responseStatus: ResponseStatus[A] = ResponseStatus[A](status)

  implicit def fstringComplete[F[_]: Functor, H[_]](implicit lift: LiftHttp[H, F]): Completing[H, A, F[A]] =
    message.fstringComplete[H, F, A](read, Status(status))

  implicit def stringComplete[H[_]: Applicative]: Completing[H, A, A] =
    message.stringComplete[H, A](read, Status(status))

  implicit val swagger: SwaggerTypeable[A] = SwaggerTypeable[String].as[A]
}

class NoneCompleting(status: Int = 200) {
  implicit val responseStatus: ResponseStatus[this.type] = ResponseStatus(status)

  implicit def femptyComplete[H[_]: Applicative, F[_]]: Completing[H, this.type, F[this.type]] =
    message.emptyComplete[H, this.type, F[this.type]](Status(status))

  implicit def emptyComplete[H[_]: Applicative]: Completing[H, this.type, this.type] =
    message.emptyComplete[H, this.type, this.type](Status(status))

  implicit val swagger: SwaggerContent[this.type] = SwaggerContent(List(status -> None))
}
