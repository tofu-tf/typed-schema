package tschema
import ru.tinkoff.tschema.swagger.SwaggerTypeable

package object swagger {
  type Swagger[A] = SwaggerTypeable[A]
}
