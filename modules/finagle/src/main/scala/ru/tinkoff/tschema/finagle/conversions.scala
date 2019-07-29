package ru.tinkoff.tschema.finagle
import com.twitter.finagle.{Service, http}

object conversions {
  final case class FinagleServiceConvertOps[A](private val svc: Service[http.Request, A]) extends AnyVal {
    def convertTo[F[_]](implicit cvt: ConvertService[F]): F[A] = cvt.convertService(svc)
  }
}
