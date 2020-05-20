package ru.tinkoff.tschema.akkaHttp
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Directive1, Rejection, Route}
import cats.{FunctorFilter, Monad, MonoidK}

object catsInstances {
  case object FilterRejection extends Rejection

  implicit val directive1Instance = new Monad[Directive1] with MonoidK[Directive1] with FunctorFilter[Directive1] {
    def pure[A](x: A): Directive1[A]                                                  = provide(x)
    def empty[A]: Directive1[A]                                                       = Directive(_ => reject(FilterRejection))
    def flatMap[A, B](fa: Directive1[A])(f: (A) => Directive1[B]): Directive1[B]      = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: (A) => Directive1[Either[A, B]]): Directive1[B]       = f(a).flatMap {
      case Left(a1) => tailRecM(a1)(f)
      case Right(x) => pure(x)
    }
    def combineK[A](x: Directive1[A], y: Directive1[A]): Directive1[A]                = x | y
    override def filter[A](fa: Directive1[A])(f: (A) => Boolean): Directive1[A]       = fa.filter(f)
    override def map[A, B](fa: Directive1[A])(f: (A) => B): Directive1[B]             = fa.map(f)
    val functor                                                                       = this
    override def mapFilter[A, B](fa: Directive1[A])(f: A => Option[B]): Directive1[B] =
      Directive(g => fa.tapply { case Tuple1(a) => f(a).fold[Route](reject())(b => g(Tuple1(b))) })
  }
}
