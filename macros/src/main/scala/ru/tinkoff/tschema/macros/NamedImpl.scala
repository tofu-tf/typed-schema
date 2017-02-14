package ru.tinkoff.tschema.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait NamedImpl[T, Input] {
  type Output
  def produce(input: Input): Output = ???
}

object NamedImpl {
  type Aux[T, I, O] = NamedImpl[T, I] {type Output = O}

  implicit def apply[T, Input](implicit routable: NamedImpl[T, Input]): Aux[T, Input, routable.Output] = routable

  implicit def materialize[T, Input, Output]: Aux[T, Input, Output] = macro NamedImplMacros.materialize[T, Input]
}

class NamedImplMacros(val c: whitebox.Context) extends shapeless.CaseClassMacros {
  import c.universe._
  type =?>[-A, +B] = PartialFunction[A, B]

  def materialize[T: WeakTypeTag, Input: WeakTypeTag]: Tree = {
    val srcType = weakTypeOf[T]
    val inpType = weakTypeOf[Input]
    info(srcType)
    val inputWiden = inpType.dealias.widen
    info(inputWiden)
    info("class :" + srcType.typeSymbol.isClass)
    info("abstract :" + srcType.typeSymbol.isAbstract)
    info(extractMethods(srcType))
    info("union: " + extractUnion(inputWiden))
    warning("asd")
    warning("1231231")

    q"""new _root_.ru.tinkoff.tschema.macros.NamedImpl[$srcType, $inpType]{
        import shapeless._
        type Output = $srcType
        override def toString = "Hello from Generic materialize\n"
     }"""
  }

  def info[A](u: A) = c.info(c.enclosingPosition, u.toString, force = true)
  def warning(msg: String) = c.warning(c.enclosingPosition, msg)

  def extractMethods(tpe: Type) =
    if (!tpe.typeSymbol.isClass || !tpe.typeSymbol.isAbstract) List()
    else tpe.decls.collect {
      case s: MethodSymbol ⇒ s.name ->
                             (s.paramLists.map(lst ⇒ lst.map(p ⇒ p.name → p.typeSignature)) -> s.returnType)
    }.toList

  def extractUnion(tpe: Type) = coproductElements(tpe)
                                .collect { case FieldType(KeyName(name), value) ⇒ name -> extractList(value) }

  def extractList(tpe: Type) = hlistElements(tpe)
                               .collect { case FieldType(KeyName(name), value) ⇒ name -> value }

  object KeyName {
    def unapply(tpe: Type): Option[String] = tpe match {
      case NamedSymbol(ConstantType(Constant(name: String))) ⇒ Some(name)
      case _ ⇒ None
    }
  }

  object NamedSymbol {
    def unapply(tpe: Type) = tpe match {
      case RefinedType(List(sym, tag, _*), _)
        if sym == symbolTpe ⇒ tag.typeArgs.headOption
      case _ ⇒ None
    }
  }
}

