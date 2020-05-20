package ru.tinkoff.tschema.macros
import shapeless.{ReprTypes, tag}
import shapeless.tag.@@

trait SingletonMacros extends ReprTypes {
  import c.universe._

  val SymTpe = typeOf[scala.Symbol]

  object SingletonTypeStr {
    def unapply(tpe: Type): Option[String] =
      tpe.dealias match {
        case ConstantType(Constant(s: scala.Symbol)) => Some(s.toString)

        case ConstantType(c: Constant) => Some(c.toString)

        case SingletonSymbolType(c) => Some(s"'$c")

        case _ => None
      }
  }

  object SingletonSymbolType {
    val atatTpe = typeOf[@@[_, _]].typeConstructor

    val TaggedSym = typeOf[tag.Tagged[_]].typeConstructor.typeSymbol

    def apply(s: String): Type = appliedType(atatTpe, List(SymTpe, c.internal.constantType(Constant(s))))

    def unapply(t: Type): Option[String] =
      t match {
        case RefinedType(List(SymTpe, TypeRef(_, TaggedSym, List(ConstantType(Constant(s: String))))), _) => Some(s)
        case _                                                                                            => None
      }
  }
}
