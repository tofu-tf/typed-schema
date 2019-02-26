package ru.tinkoff.tschema

import org.scalacheck.Arbitrary
import org.scalactic.Equality
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import ru.tinkoff.tschema.ForAllTypes.Checker
import ru.tinkoff.tschema.akkaHttp.ParamSource.{Cookie, Form, Query}
import ru.tinkoff.tschema.akkaHttp.{Param, SingleParam, ParamSource}
import shapeless._

import scala.reflect.runtime.universe._

trait ForAllTypes[S <: ParamSource, L] {
  def check(checker: Checker[S]): Unit
}
object ForAllTypes {
  def apply[L] = new Applier[L]

  class Applier[L] {
    def apply[S <: ParamSource](checker: Checker[S])(implicit forAll: ForAllTypes[S, L]) = forAll.check(checker)
  }

  abstract class Checker[S <: ParamSource] {
    def check[T](implicit f: SingleParam[S, T], arb: Arbitrary[T], tt: TypeTag[T], eq: Equality[T]): Unit
  }

  implicit def hnilChecks[S <: ParamSource] = new ForAllTypes[S, HNil] {
    def check(checker: Checker[S]) = ()
  }

  implicit def hconsChecks[S <: ParamSource, A, Tail <: HList](implicit F: SingleParam[S, A],
                                                          arb: Arbitrary[A],
                                                          tt: TypeTag[A],
                                                          eqt: Equality[A],
                                                          tail: ForAllTypes[S, Tail]) =
    new ForAllTypes[S, A :: Tail] {
      def check(checker: Checker[S]): Unit = {
        checker.check[A](F, arb, tt, eqt)
        tail.check(checker)
      }
    }

  implicit def tupleCheck[S <: ParamSource, T, L <: HList](implicit gen: Generic.Aux[T, L], forAll: ForAllTypes[S, L]) =
    new ForAllTypes[S, T] {
      def check(checker: Checker[S]): Unit = forAll.check(checker)
    }
}

trait ParamSpecLow[S <: ParamSource] {
  implicit def listParam[A: SingleParam[S, ?]]: SingleParam[S, List[A]] = Param.separated(",")
}

abstract class ParamSpec[S <: ParamSource] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with ParamSpecLow[S] {
  implicit def listList[A: SingleParam[S, ?]]: SingleParam[S, List[List[A]]] = Param.separated[S, List[A]](";")
  def fromParam[T](s: String)(implicit f: SingleParam[S, T])                 = f.applyOpt(Some(s))
}

class FromFormParamSpec extends ParamSpec[ParamSource.Form] {
  ForAllTypes[(Int, Long, String, BigInt, Float, Double, Boolean)](new Checker[ParamSource.Form] {
    def check[T: SingleParam[Form, ?]: Arbitrary: TypeTag: Equality]: Unit = {
      val name = typeTag[T].tpe.toString

      property(s"$name should be parsed as itself") {
        forAll((value: T) => fromParam[T](value.toString) === value)
      }

      property(s"List[$name] should be parsed as itself ") {
        forAll((list: List[T]) => fromParam[List[T]](list.mkString(",")) === list)
      }

      property(s"List[List[$name]] should be parsed as itself") {
        forAll((list2: List[List[T]]) => fromParam[List[List[T]]](list2.map(_.mkString(",")).mkString(";")) === list2)
      }
    }
  })
}

class FromQueryParamSpec extends ParamSpec[ParamSource.Query] {
  ForAllTypes[(Int, Long, String, BigInt, Float, Double, Boolean)](new Checker[ParamSource.Query] {
    def check[T: SingleParam[Query, ?]: Arbitrary: TypeTag: Equality]: Unit = {
      val name = typeTag[T].tpe.toString

      property(s"$name should be parsed as itself") {
        forAll((value: T) => fromParam[T](value.toString) === value)
      }

      property(s"List[$name] should be parsed as itself ") {
        forAll((list: List[T]) => fromParam[List[T]](list.mkString(",")) === list)
      }

      property(s"List[List[$name]] should be parsed as itself") {
        forAll((list2: List[List[T]]) => fromParam[List[List[T]]](list2.map(_.mkString(",")).mkString(";")) === list2)
      }
    }
  })
}

class FromCookieParamSpec extends ParamSpec[ParamSource.Cookie] {
  ForAllTypes[(Int, Long, String, BigInt, Float, Double, Boolean)](new Checker[ParamSource.Cookie] {
    def check[T: SingleParam[Cookie, ?]: Arbitrary: TypeTag: Equality]: Unit = {
      val name = typeTag[T].tpe.toString

      property(s"$name should be parsed as itself") {
        forAll((value: T) => fromParam[T](value.toString) === value)
      }

      property(s"List[$name] should be parsed as itself ") {
        forAll((list: List[T]) => fromParam[List[T]](list.mkString(",")) === list)
      }

      property(s"List[List[$name]] should be parsed as itself") {
        forAll((list2: List[List[T]]) => fromParam[List[List[T]]](list2.map(_.mkString(",")).mkString(";")) === list2)
      }
    }
  })
}
