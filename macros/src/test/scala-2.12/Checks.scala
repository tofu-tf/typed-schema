import ru.tinkoff.tschema.macros.NamedImpl
import shapeless._
import shapeless.labelled.FieldType
import shapeless.syntax._
import shapeless.syntax.singleton._
import shapeless.union._
import shapeless.{Witness ⇒ W}
import shapeless.labelled._

object toProduct extends Poly1 {
  implicit def labelled[T](implicit lgen: LabelledGeneric[T]): Case.Aux[T, lgen.Repr] = at[T](t ⇒ lgen.to(t))
}

object Checks {


  import scala.concurrent.Future
  sealed trait DatabaseInput
  final case class put(id: Int, value: String) extends DatabaseInput
  final case class read(id: Long, session: Long) extends DatabaseInput

  val inputCopro = LabelledGeneric[DatabaseInput].to(put(1, "adasd"))
  val inputGen = inputCopro.mapValues(toProduct)

  LabelledGeneric[put]

  trait DatabaseService {
    def put(value: String, id: Int): Future[Unit]
    def read(id: Long): Future[Long]
    def zzz(u: Double): Unit
  }

  NamedImpl[DatabaseService, inputGen.type]
}
