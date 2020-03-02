package ru.tinkoff.tschema.swagger
import ru.tinkoff.tschema.typeDSL._
final case class MethodDeclare[method](method: OpenApi.Method)
object MethodDeclare {
  implicit val checkGet: MethodDeclare[Get]         = MethodDeclare(OpenApi.Method.get)
  implicit val checkPost: MethodDeclare[Post]       = MethodDeclare(OpenApi.Method.post)
  implicit val checkDelete: MethodDeclare[Delete]   = MethodDeclare(OpenApi.Method.delete)
  implicit val checkPut: MethodDeclare[Put]         = MethodDeclare(OpenApi.Method.put)
  implicit val checkOptions: MethodDeclare[Options] = MethodDeclare(OpenApi.Method.options)
  implicit val checkHead: MethodDeclare[Head]       = MethodDeclare(OpenApi.Method.head)
  implicit val checkPatch: MethodDeclare[Patch]     = MethodDeclare(OpenApi.Method.patch)
}
