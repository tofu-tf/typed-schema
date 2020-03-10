package ru.tinkoff.tschema.swagger
import scala.collection.immutable

trait AdditionalSwaggerInstances extends LowLevelSwaggerTypeable {
  final implicit def immutableseqTypeable[T: SwaggerTypeable]: SwaggerTypeable[immutable.Seq[T]] = seq[immutable.Seq, T]
}
