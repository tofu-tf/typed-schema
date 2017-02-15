import ru.tinkoff.tschema.macros.NamedImpl
import shapeless._
import shapeless.labelled.FieldType
import shapeless.syntax._
import shapeless.syntax.singleton._
import shapeless.union._
import shapeless.{Witness ⇒ W}
import shapeless.labelled._
import shapeless.tag.{@@, Tagged}

object toProduct extends Poly1 {
  implicit def labelled[T](implicit lgen: LabelledGeneric[T]): Case.Aux[T, lgen.Repr] = at[T](t ⇒ lgen.to(t))
}

object Checks extends App {

  import scala.concurrent.Future
  sealed trait DatabaseInput
  final case class put(id: Long, value: String) extends DatabaseInput
  final case class read(id: Long, session: Long) extends DatabaseInput
//  final case class delete(id: Long) extends DatabaseInput

  val inputCopro = LabelledGeneric[DatabaseInput].to(read(2, 123))
  val inputGen = inputCopro.mapValues(toProduct)

  LabelledGeneric[put]

  trait DatabaseService {
    def put(value: String)(id: Long) = s"$value $id"
    def read(id: AnyVal) = println(id)
    def zzz(u: Double): Unit = ()
  }

  val result = NamedImpl[DatabaseService, inputGen.type]
  print(result.produce(inputGen, new DatabaseService {}))
}

