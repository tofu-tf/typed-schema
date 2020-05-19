package ru.tinkoff.tschema.finagle

import cats.instances.list._
import cats.syntax.order._
import cats.{Monoid, Order}
import com.twitter.finagle.http
import com.twitter.finagle.http.Response
import com.twitter.finagle.http.Status._
import ru.tinkoff.tschema.finagle.Rejection.{MalformedParam, MissingParam}
import ru.tinkoff.tschema.param.ParamSource

final case class Rejection(
    priority: Double,
    status: http.Status = NotFound,
    path: String = "",
    wrongMethod: List[String] = Nil,
    missing: List[MissingParam] = Nil,
    malformed: List[MalformedParam] = Nil,
    unauthorized: Boolean = false,
    messages: List[String] = Nil
) {
  def withPath(p: String): Rejection   = copy(path = p)
  def addMessage(s: String): Rejection = copy(messages = s :: messages)
  def message: String                  = messages.headOption.getOrElse {
    (Iterator(s"at $path") ++
      Iterator(
        "incorrect methods"    -> wrongMethod,
        "missing parameters"   -> missing.map(p => s"${p.name} in ${p.source}"),
        "malformed parameters" -> malformed.map(p => s"${p.name} in ${p.source} with ${p.error}")
      ).collect { case (m, ms) if ms.nonEmpty => ms.mkString(m + " ", ",", "") } ++
      Iterator("unauthorized").filter(_ => unauthorized)).mkString("\n")
  }
}

object Rejection {
  val notFound                                                         = Rejection(0)
  def wrongMethod(method: String)                                      = Rejection(1, MethodNotAllowed, wrongMethod = List(method))
  def missingParam(name: String, source: ParamSource)                  =
    Rejection(3, BadRequest, missing = List(MissingParam(name, source)))
  def malformedParam(name: String, error: String, source: ParamSource) =
    Rejection(4, BadRequest, malformed = List(MalformedParam(name, error, source)))
  def body(error: String)                                              = Rejection(5, BadRequest, messages = List(error))
  val unauthorized                                                     = Rejection(6, Unauthorized)

  case class MissingParam(name: String, source: ParamSource)
  case class MalformedParam(name: String, error: String, source: ParamSource)

  implicit val rejectionMonoid: Monoid[Rejection] = new Monoid[Rejection] {
    def empty: Rejection                               = notFound
    def combine(x: Rejection, y: Rejection): Rejection =
      if (x < y) combine(y, x)
      else
        Rejection(
          x.priority,
          x.status,
          x.path,
          x.wrongMethod ::: y.wrongMethod,
          x.missing ::: y.missing,
          x.malformed ::: y.malformed,
          x.unauthorized || y.unauthorized,
          x.messages ::: y.messages
        )
  }

  implicit val order: Order[Rejection] =
    (x, y) =>
      x.priority compare y.priority match {
        case 0 => x.path.length compare y.path.length
        case i => i
      }

  trait Handler {
    def apply(rej: Rejection): Response
  }

  val defaultHandler: Handler = rej => {
    val response = http.Response(rej.status)
    response.setContentString(rej.message)
    response
  }
}
