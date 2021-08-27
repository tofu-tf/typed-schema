package ru.tinkoff.tschema.example

import cats.{Functor, Monad}
import io.circe.Encoder
import derevo.tethys.{tethysReader, tethysWriter}
import ru.tinkoff.tschema.finagle.{LiftHttp, Routed}
import ru.tinkoff.tschema.swagger.Swagger
import ru.tinkoff.tschema.common.Name
import derevo.circe.{decoder, encoder}
import derevo.derive
import ru.tinkoff.tschema.finagle.CompleteIn
import ru.tinkoff.tschema.finagle.util.message
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{Mapper, ToList}
import cats.syntax.applicative._
import cats.syntax.functor._
import tethys._
import tethys.jackson._

@derive(tethysWriter, tethysReader, Swagger)
final case class DebugParams[T](value: T, params: Map[String, String])

object DebugParams {
  implicit def routable[In <: HList, T: JsonWriter, L <: HList, H[_]: Monad](implicit
      im: Mapper.Aux[InputParamMap.type, In, L],
      tl: ToList[L, (String, String)]
  ): CompleteIn[H, In, DebugParams[T], T] =
    (res, in) => message.jsonResponse(DebugParams(res, im(in).toList.toMap).asJson).pure[H]

  implicit def routableF[In <: HList, T: JsonWriter, L <: HList, H[_]: Monad, F[_]](implicit
      im: Mapper.Aux[InputParamMap.type, In, L],
      tl: ToList[L, (String, String)],
      lift: LiftHttp[H, F]
  ): CompleteIn[H, In, DebugParams[T], F[T]] =
    (fres, in) => lift(fres).map(res => message.jsonResponse(DebugParams(res, im(in).toList.toMap).asJson))
}

trait InputParamMap[L <: HList] {
  def apply(l: L): Map[String, String]
}

trait LowLevelInputParamMap {
  implicit def simpleCons[L <: HList, A](implicit
      tail: InputParamMap[L]
  ): InputParamMap[A :: L] =
    l => tail(l.tail)
}

object InputParamMap extends Poly1 {
  implicit def consume[K: Name, V] = at[FieldType[K, V]](v => Name[K].string -> v.toString)
}
