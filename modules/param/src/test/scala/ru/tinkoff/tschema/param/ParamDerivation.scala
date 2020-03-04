package tschema.param
import derevo.derive

object ParamDerivation {
  @derive(HttpParam)
  final case class Person(
      firstName: String,
      lastName: Option[String],
      age: Int,
      infant: Option[Person]
  )
}
