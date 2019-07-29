package ru.tinkoff.tschema.example

import io.circe.Encoder
import org.manatki.derevo.tethysInstances.{tethysReader, tethysWriter}
//import io.circe.syntax._
import org.manatki.derevo.circeDerivation.{decoder, encoder}
import org.manatki.derevo.derive
import org.manatki.derevo.tschemaInstances.swagger
import ru.tinkoff.tschema.common.Name
import ru.tinkoff.tschema.finagle.CompleteIn
import ru.tinkoff.tschema.finagle.util.message
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{Mapper, ToList}
import cats.syntax.applicative._
import tethys._
import tethys.jackson._

@derive(tethysWriter, tethysReader, swagger)
final case class DebugParams[T](value: T, params: Map[String, String])

object DebugParams {
  implicit def routable[In <: HList, T: JsonWriter, L <: HList](
      implicit
      im: Mapper.Aux[InputParamMap.type, In, L],
      tl: ToList[L, (String, String)]
  ): CompleteIn[Http, In, DebugParams[T], T] =
    (res, in) => message.jsonResponse(DebugParams(res, im(in).toList.toMap).asJson).pure[Http]
}

trait InputParamMap[L <: HList] {
  def apply(l: L): Map[String, String]
}

trait LowLevelInputParamMap {
  implicit def simpleCons[L <: HList, A](
      implicit tail: InputParamMap[L]
  ): InputParamMap[A :: L] =
    l => tail(l.tail)
}

object InputParamMap extends Poly1 {
  implicit def consume[K: Name, V] = at[FieldType[K, V]](v => Name[K].string -> v.toString)
}
