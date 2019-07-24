package ru.tinkoff.tschema
package examples

import akka.http.scaladsl.server.Directives
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.Encoder
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.akkaHttp.RoutableIn
import shapeless._
import shapeless.labelled.FieldType

@derive(encoder, decoder, swagger)
final case class DebugParams[T](value: T, params: Map[String, String])

object DebugParams {
  implicit def routable[In <: HList, T: Encoder](implicit im: InputParamMap[In]): RoutableIn[In, T, DebugParams[T]] =
    (in, res) => Directives.complete(DebugParams[T](res, im(in)))
}

trait InputParamMap[L <: HList] {
  def apply(l: L): Map[String, String]
}

trait LowLevelInputParamMap {
  implicit def simpleCons[L <: HList, A](implicit tail: InputParamMap[L]): InputParamMap[A :: L] =
    l => tail(l.tail)
}

object InputParamMap extends LowLevelInputParamMap {
  def apply[L <: HList](implicit im: InputParamMap[L]): InputParamMap[L] = im

  implicit val hnil: InputParamMap[HNil] = _ => Map.empty

  implicit def keyValueCons[L <: HList, K <: Symbol, V](
      implicit witness: Witness.Aux[K],
      tail: InputParamMap[L]
  ): InputParamMap[FieldType[K, V] :: L] =
    l => tail(l.tail) + (witness.value.name -> l.head.toString)
}
