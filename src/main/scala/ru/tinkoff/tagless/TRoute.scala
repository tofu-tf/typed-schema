package ru.tinkoff.tagless

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.{HttpMethod, HttpResponse}
import ru.tinkoff.tschema.swagger.SwaggerType
import shapeless._

sealed trait ResultBuild[⇰[-I, +O], Res]
trait Cons[T, I] extends DepFn2[T, I]
trait Comb[A, B] extends DepFn2[A, B]

sealed trait TRoute[⇰[-I, +O]] {
  type Result[+O]
  def result[O](method: HttpMethod, res: ⇒ Result[O])(implicit build: ResultBuild[⇰, O]): HNil ⇰ O
  def param[T, I, O](source: ParamSource, next: I ⇰ O)(implicit cons: Cons[T, I]): cons.Out ⇰ O
  def combine[I1, I2, O1, O2](left: I1 ⇰ O1, right: I2 ⇰ O2)(implicit combI: Comb[I1, I2], combO: Comb[O1, O2]): combI.Out ⇰ combO.Out
}

sealed trait ParamSource

object ParamSource {
  case object Query extends ParamSource
  case object Path extends ParamSource
  case object Forum extends ParamSource
  case object Header extends ParamSource
}
