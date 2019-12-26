package tschema
import tschema.Decompose.{Cons, Last}

/** special wrapper type making reasoning about request with multiple status outcomes easier */
trait Composite[A]

/** microtypeclass for binding special types with the corresponding status */
trait ResponseStatus[T] {
  def status: Int
}

object ResponseStatus {
  final case class Impl[T](status: Int) extends ResponseStatus[T]

  def apply[T](x: Int): ResponseStatus[T] = Impl(x)

  final def default[T]: ResponseStatus[T] = ResponseStatus[T](200)

  implicit val noneStatus: ResponseStatus[None.type]        = ResponseStatus(404)
  implicit def leftStatus[A, B]: ResponseStatus[Left[A, B]] = ResponseStatus(400)
  implicit def rightStatus[A, B](implicit right: ResponseStatus[B]): ResponseStatus[Right[A, B]] = ResponseStatus(
    right.status
  )
}

sealed trait Decompose[A] {
  type Self
  def self: Self
}

private case object DecomposeLast extends Last[Any] {
  def self = this
}

object Decompose {
  def make[A] = new Applier[A, Last[A]](nil)

  def apply[A](implicit decompose: Decompose[A]): Aux[A, decompose.Self] = decompose

  type Aux[A, D] = Decompose[A] { type Self = D }

  class Applier[A, D <: Decompose[A]](private val d: D) extends AnyVal {
    def apply[B](pf: PartialFunction[A, B]) =
      new Applier[A, Cons[A, B, D]](new Cons[A, B, D] {
        def tryHead(a: A): Option[B] = if (pf.isDefinedAt(a)) Some(pf(a)) else None
        def next: D                  = d
      })

    def result: D = d
  }

  def nil[A]: Last[A] = DecomposeLast.asInstanceOf[Last[A]]

  trait Cons[A, H, D] extends Decompose[A] {
    type Self = Cons[A, H, D]
    def tryHead(a: A): Option[H]
    def next: D
    def self = this
  }

  sealed trait Last[A] extends Decompose[A] {
    type Self = Last[A]
  }

  type Arb
  type Arb1

  type OptInst[A] = Cons[Option[A], NotFound.type, Cons[Option[A], A, Last[Option[A]]]]
  private val optionArb: OptInst[Arb] =
    Decompose.make[Option[Arb]] { case Some(a) => a } { case None => NotFound } result
  implicit def optionInstance[A]: OptInst[A] = optionArb.asInstanceOf[OptInst[A]]

  type EitherInst[A, B] = Cons[Either[A, B], Left[A, B], Cons[Either[A, B], B, Last[Either[A, B]]]]
  private val eitherArb: EitherInst[Arb, Arb1] =
    Decompose.make[Either[Arb, Arb1]] { case Right(x) => x } { case l @ Left(_) => l } result
  implicit def eitherInstance[A, B]: EitherInst[A, B] = eitherArb.asInstanceOf[EitherInst[A, B]]

  case object NotFound
}
