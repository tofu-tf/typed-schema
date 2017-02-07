package ru.tinkoff.travel.schema.serve
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.Equality
import ru.tinkoff.travel.schema.serve.ForAllTypes.Checker
import shapeless._

import scala.reflect.runtime.universe._

trait ForAllTypes[F[x], L] {
  def check(checker: Checker[F]): Unit
}
object ForAllTypes {
  def apply[L] = new Applier[L]

  class Applier[L] {
    def apply[F[x]](checker: Checker[F])(implicit forAll: ForAllTypes[F, L]) = forAll.check(checker)
  }

  abstract class Checker[F[x]] {
    def check[T](implicit f: F[T], arb: Arbitrary[T], tt: TypeTag[T], eq: Equality[T]): Unit
  }

  implicit def hnilChecks[F[x]] = new ForAllTypes[F, HNil] {
    def check(checker: Checker[F]) = ()
  }

  implicit def hconsChecks[F[x], A, Tail <: HList]
  (implicit F: F[A], arb: Arbitrary[A], tt: TypeTag[A], eqt: Equality[A], tail: ForAllTypes[F, Tail]) =
    new ForAllTypes[F, A :: Tail] {
      def check(checker: Checker[F]): Unit = {
        checker.check[A](F, arb, tt, eqt)
        tail.check(checker)
      }
    }

  implicit def tupleCheck[F[x], T, L <: HList]
  (implicit gen: Generic.Aux[T, L], forAll: ForAllTypes[F, L]) =
    new ForAllTypes[F, T] {
      def check(checker: Checker[F]): Unit = forAll.check(checker)
    }
}

abstract class FromParamSpec[F[x] <: FromParam[x]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  def fromParam[T](s: String)(implicit f: F[T]) = f(s)
}

class FromFormParamSpec extends FromParamSpec[FromFormField] {
  ForAllTypes[(Int, Long, String, BigInt, Float, Double, Boolean)](
    new Checker[FromFormField] {
      def check[T: FromFormField : Arbitrary : TypeTag : Equality]: Unit = {
        val name = typeTag[T].tpe.toString

        property(s"$name should be parsed as itself") {
          forAll((value: T) ⇒ fromParam[T](value.toString) === value)
        }

        property(s"List[$name] should be parsed as itself ") {
          forAll((list: List[T]) ⇒ fromParam[List[T]](list.mkString(",")) === list)
        }

        property(s"List[List[$name]] should be parsed as itself") {
          forAll((list2: List[List[T]]) ⇒ fromParam[List[List[T]]](list2.map(_.mkString(",")).mkString(";")) === list2)
        }
      }
    })
}
