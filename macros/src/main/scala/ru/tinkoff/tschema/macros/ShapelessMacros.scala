package ru.tinkoff.tschema.macros
import shapeless.ReprTypes

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

trait ShapelessMacros extends ReprTypes with MacroMessages with SymbolMacros{
  val c: blackbox.Context
  import c.universe._

  def unfoldCompoundTpe(compoundTpe: Type, nil: Type, cons: Type): List[Type] = {
    @tailrec
    def loop(tpe: Type, acc: List[Type]): List[Type] =
      tpe.dealias match {
        case TypeRef(_, consSym, List(hd, tl))
          if consSym.asType.toType.typeConstructor =:= cons => loop(tl, hd :: acc)
        case `nil` => acc
        case other => abort(s"Bad compound type $compoundTpe")
      }
    loop(compoundTpe, Nil).reverse
  }

  def hlistElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, hnilTpe, hconsTpe)

  def extractRecord(tpe: Type): List[Option[(String, Type)]] =
    hlistElements(tpe).map {
      case FieldType(KeyName(name), value) ⇒ Some(name -> value)
      case _ => None
    }

  object FieldType {
    import internal._

    def apply(kTpe: Type, vTpe: Type): Type =
      refinedType(List(vTpe, typeRef(keyTagTpe, keyTagTpe.typeSymbol, List(kTpe, vTpe))), NoSymbol)

    def unapply(fTpe: Type): Option[(Type, Type)] = {
      val KeyTagPre = keyTagTpe
      fTpe.dealias match {
        case RefinedType(List(v0, TypeRef(pre, sym, List(k, v1))), _) if sym.asType.toType.typeConstructor =:= KeyTagPre && v0 =:= v1 => Some((k, v0))
        case _ => None
      }
    }
  }

}
