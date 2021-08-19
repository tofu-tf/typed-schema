package ru.tinkoff.tschema.example.sample
import cats.Monad
import ru.tinkoff.tschema.example.Example
import cats.syntax.applicative._

trait SampleString[F[_]] {
  def mutate(value: Long): F[String]
  def concat(l: String, r: String): F[String]
}

object SampleString {
  implicit def instance[F[_]: Monad]: SampleString[F] = new SampleString[F] {
    def mutate(value: Long)          = java.lang.Long.toBinaryString(value).pure[F]
    def concat(l: String, r: String) = (l + r).pure[F]
  }
}

trait SampleOps[F[_]] {
  def combine(x: Client, y: Int): F[Combine]

  def sum(x: Client, y: Int): F[Int]

  def sqrtMean(a: Double, b: Double): F[Double]

  def stats(body: Seq[BigDecimal]): F[StatsRes]

  def statsq(num: Seq[BigDecimal]): F[StatsRes]
}

object SampleOps {
  implicit def instance[F[_]: Monad]: SampleOps[F] = new SampleOps[F] {
    def combine(x: Client, y: Int) =
      Combine(
        CombSource(x.value, y),
        CombRes(mul = x.value * y, sum = x.value + y)
      ).pure[F]

    def sum(x: Client, y: Int) = (x.value + y).pure[F]

    def sqrtMean(a: Double, b: Double) = Math.sqrt((a * a + b * b) / 2).pure[F]

    def stats(body: Seq[BigDecimal]) = {
      val mean   = body.sum / body.size
      val mid    = body.size / 2
      val median =
        if (body.size % 2 == 1) body(mid) else (body(mid) + body(mid - 1)) / 2
      val std    = body.view.map(x => x * x).sum / body.size - mean * mean
      StatsRes(mean, std, median)
    }.pure[F]

    def statsq(num: Seq[BigDecimal]): F[StatsRes] = stats(num)
  }
}
