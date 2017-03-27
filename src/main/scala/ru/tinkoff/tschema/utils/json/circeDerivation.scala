package ru.tinkoff.tschema.utils.json

import io.circe._
import shapeless._
import shapeless.labelled.FieldType

import scala.language.higherKinds
object circeDerivation {
  @deprecated("use standard derivation")
  def deriveObjEncoder[T](implicit enc: Lazy[DerivedCirceEncoder[T]]) = new ObjectEncoder[T]{
    def encodeObject(a: T): JsonObject = enc.value.encodeObject(a)
  }
}

trait DerivedCirceEncoder[T] extends ObjectEncoder[T] {
  def encodeObject(a: T): JsonObject
}

trait LowPriorityCirceEncoder {
  implicit def consEncoder[A, S <: Symbol, L <: HList]
  (implicit head: Lazy[Encoder[A]], tail: DerivedCirceEncoder[L], witness: Witness.Aux[S]) = new DerivedCirceEncoder[FieldType[S, A] :: L] {
    def encodeObject(obj: ::[FieldType[S, A], L]): JsonObject =
      tail.encodeObject(obj.tail).add(witness.value.name, head.value.apply(obj.head))
  }
}

object DerivedCirceEncoder extends LowPriorityCirceEncoder {
  implicit val nilEncoder = new DerivedCirceEncoder[HNil] {
    override def encodeObject(a: HNil): JsonObject = JsonObject.empty
  }

  implicit val cnilEncoder = new DerivedCirceEncoder[CNil] {
    override def encodeObject(a: CNil): JsonObject = a.impossible
  }

  implicit def consNullableEncoder[A, S <: Symbol, L <: HList]
  (implicit head: Lazy[Encoder[A]], tail: DerivedCirceEncoder[L], witness: Witness.Aux[S], nullable: Nullable[A]) =
    new DerivedCirceEncoder[FieldType[S, A] :: L] {
      def encodeObject(obj: ::[FieldType[S, A], L]): JsonObject = {
        val tailEnc = tail.encodeObject(obj.tail)
        (obj.head: A) match {
          case nullable.zero ⇒ tailEnc
          case x ⇒ tailEnc.add(witness.value.name, head.value.apply(x))
        }
      }
    }

  implicit def orEncoder[left, right <: Coproduct]
  (implicit left: DerivedCirceEncoder[left], right: DerivedCirceEncoder[right]) = new DerivedCirceEncoder[left :+: right] {
    def encodeObject(a: left :+: right): JsonObject = a match {
      case Inl(l: left@unchecked) ⇒ left.encodeObject(l)
      case Inr(r: right@unchecked) ⇒ right.encodeObject(r)
    }
  }

  implicit def deriveHListEncoder[T, L <: HList]
  (implicit lgen: LabelledGeneric.Aux[T, L], enc: DerivedCirceEncoder[L]) = new DerivedCirceEncoder[T]{
    def encodeObject(a: T): JsonObject = enc.encodeObject(lgen.to(a))
  }

  implicit def deriveCoproductEncoder[T, C <: Coproduct]
  (implicit gen: Generic.Aux[T, C], enc: DerivedCirceEncoder[C]) = new DerivedCirceEncoder[T]{
    def encodeObject(a: T): JsonObject = enc.encodeObject(gen.to(a))
  }
}

case class Nullable[X](zero: X) extends AnyVal

object Nullable {
  implicit def optionNullable[X] = Nullable[Option[X]](None)
  implicit def vectorNullable[X] = Nullable[Vector[X]](Vector.empty)
  implicit def mapNullable[K, V] = Nullable[Map[K, V]](Map.empty)
}
