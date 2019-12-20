package ru.tinkoff.tschema.akkaHttp

import ForAllTypes.Checker
import org.scalacheck.Arbitrary
import org.scalactic.Equality
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import ru.tinkoff.tschema.param.ParamSource._
import ru.tinkoff.tschema.param.{Param, ParamSource, SingleParam}
import shapeless._
import scala.reflect.runtime.universe._

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

trait ForAllTypes[S >: All <: ParamSource, L] {
  def check(checker: Checker[S]): Unit
}
object ForAllTypes {
  def apply[L] = new Applier[L]

  class Applier[L] {
    def apply[S >: All <: ParamSource](checker: Checker[S])(implicit forAll: ForAllTypes[S, L]) = forAll.check(checker)
  }

  abstract class Checker[S >: All <: ParamSource] {
    def check[T](implicit f: SingleParam[S, T], arb: Arbitrary[T], tt: TypeTag[T], eq: Equality[T]): Unit
  }

  implicit def hnilChecks[S >: All <: ParamSource] = new ForAllTypes[S, HNil] {
    def check(checker: Checker[S]) = ()
  }

  implicit def hconsChecks[S >: All <: ParamSource, A, Tail <: HList](implicit F: SingleParam[S, A],
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

  implicit def tupleCheck[S >: All <: ParamSource, T, L <: HList](implicit gen: Generic.Aux[T, L], forAll: ForAllTypes[S, L]) =
    new ForAllTypes[S, T] {
      def check(checker: Checker[S]): Unit = forAll.check(checker)
    }
}

trait ParamSpecLow[S >: All <: ParamSource] {
  val byColon = "\\,".r
  implicit def listParam[A: SingleParam[S, *]]: SingleParam[S, List[A]] = {
    Param.separated[S, A](byColon)
  }
}

abstract class ParamSpec[S >: All <: ParamSource]
    extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with ParamSpecLow[S] {
  val bySemicolon                                                            = ";".r
  implicit def listList[A: SingleParam[S, *]]: SingleParam[S, List[List[A]]] = Param.separated[S, List[A]](bySemicolon)
  def fromParam[T](s: String)(implicit f: SingleParam[S, T])                 = f.applyOpt(Some(s))
}

class FromFormParamSpec extends ParamSpec[ParamSource.Form] {
  ForAllTypes[(Int, Long, String, BigInt, Float, Double, Boolean)](new Checker[ParamSource.Form] {
    def check[T: SingleParam[Form, *]: Arbitrary: TypeTag: Equality]: Unit = {
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
    def check[T: SingleParam[Query, *]: Arbitrary: TypeTag: Equality]: Unit = {
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
    def check[T: SingleParam[Cookie, *]: Arbitrary: TypeTag: Equality]: Unit = {
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
