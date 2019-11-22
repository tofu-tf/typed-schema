package ru.tinkoff.tschema.custom
import ru.tinkoff.tschema.finagle.Complete
import ru.tinkoff.tschema.swagger.{MkSwagger, SwaggerContent}
import ru.tinkoff.tschema.typeDSL

class ExceptResult[E, A]

object ExceptResult {
  implicit def exceptComplete[F[_], R, E, A](
      implicit result: ExceptComplete[F, R, E, A]
  ): Complete[F, ExceptResult[E, R], A] = result

  implicit def exceptSwagger[E, R](
      implicit success: MkSwagger[typeDSL.Complete[R]],
      fail: SwaggerContent[E]
  ): MkSwagger[typeDSL.Complete[ExceptResult[E, R]]] =
    success.addContent(fail).as[typeDSL.Complete[ExceptResult[E, R]]]
}