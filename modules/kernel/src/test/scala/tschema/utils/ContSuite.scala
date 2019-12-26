package tschema.utils
import cats.Eval
import cats.data.ReaderT
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContSuite extends AnyFlatSpec with Matchers {
  "traverse cont" should "not overflow stack in simple" in {
    cont
      .traverseCont[Int, Long, String, Eval](List.range(10000, 0, -1))((a, kb) => kb(a * 2))(ls =>
        Eval.now(ls.mkString("[", ",", "]")))
      .value shouldBe Iterator.range(20000, 0, -2).mkString("[", ",", "]")
  }

  it should "not overflow stack in reader" in {
    cont
      .traverseCont[Int, (Long, Long), String, ReaderT[Eval, Long, *]](List.range(10000, 0, -1))((a, kb) =>
        ReaderT.ask[Eval, Long].flatMap(i => kb(a * 2, i).local(_ + 1)))(ls => ReaderT.pure(ls.mkString("[", ",", "]")))
      .run(0L)
      .value shouldBe Iterator.range(20000, 0, -2).zip(Iterator.range(0, 10000)).mkString("[", ",", "]")
  }
}
