package ru.tinkoff.tschema

import ru.tinkoff.tschema.macros.NamedImpl
import shapeless.ops.{coproduct, hlist, union}
import shapeless.union._
import shapeless.{Witness => W, _}

object toProduct extends Poly1 {
  implicit def labelled[T](implicit lgen: LabelledGeneric[T]): Case.Aux[T, lgen.Repr] = at[T](t => lgen.to(t))
}

object NamedImplSuite extends App {
  sealed trait DatabaseInput
  final case class put(id: Long, value: String)  extends DatabaseInput
  final case class read(id: Long, session: Long) extends DatabaseInput
//  final case class delete(id: Long) extends DatabaseInput

  val inputCopro = LabelledGeneric[DatabaseInput].to(read(2, 123))
  val inputGen   = inputCopro.mapValues(toProduct)

  LabelledGeneric[put]

  object DatabaseService {
    def put(value: String)(id: Long) = s"$value $id"
    def read(id: AnyVal)             = println(id)
    def zzz(u: Double): Unit         = ()
  }

  val result = NamedImpl[DatabaseService.type, inputGen.type]
  val values = union.Values[result.Output]
  val align  = coproduct.Align[values.Out, String :+: Unit :+: CNil]
  val keys   = union.Keys[result.Output]
  val names  = hlist.Reify[keys.Out]
  println(result.description)
  println(names().toList)
  println(result.produce(inputGen, DatabaseService))
}
