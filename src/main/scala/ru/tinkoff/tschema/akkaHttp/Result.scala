package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._
import ru.tinkoff.tschema.typeDSL._

trait Result[T] {
  type Out
  def directive: Directive0
}

object Result {
  type Aux[T, O] = Result[T] {type Out = O}
  def apply[T](implicit result: Result[T]): Aux[T, result.Out] = result

  private def mkResult[X, F[_]](dir: Directive0): Aux[F[X], X] = new Result[F[X]] {
    type Out = X
    override def directive: Directive0 = pathEnd & dir
  }

  implicit def getResult[X] = mkResult[X, Get](get)
  implicit def postResult[X] = mkResult[X, Post](post)
  implicit def putResult[X] = mkResult[X, Put](put)
  implicit def deleteResult[X] = mkResult[X, Delete](delete)
  implicit def headResult[X] = mkResult[X, Head](head)
  implicit def optionsResult[X] = mkResult[X, Options](options)
  implicit def patchResult[X] = mkResult[X, Patch](patch)
}
