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
  import NamedImplMacros._
  type =?>[-A, +B] = PartialFunction[A, B]

  def materialize[T: WeakTypeTag, Input: WeakTypeTag]: Tree = {

    val applier = new Applier(weakTypeOf[T], weakTypeOf[Input])
    import applier._
    info(impl)

    info(inputWiden)
    info(s"methods: $methods")
    info(s"union  $union")
    if (!verifyTypes) abort("could not satisfy input type with implementation type")

    q"""new _root_.ru.tinkoff.tschema.macros.NamedImpl[$impl, $input]{
        import shapeless._
        type Output = $output
        override def toString = "Hello from Generic materialize\n"
     }"""
  }

  def info[A](u: A) = c.info(c.enclosingPosition, u.toString, force = true)
  def warning(msg: String) = c.warning(c.enclosingPosition, msg)

  def extractMethods(tpe: Type): NList[List[NList[Type]]] =
    if (!tpe.typeSymbol.isClass || !tpe.typeSymbol.isAbstract) List()
    else tpe.decls.collect {
      case s: MethodSymbol ⇒ s.name.decodedName.toString ->
                             s.paramLists.map(lst ⇒ lst.map(p ⇒ p.name.decodedName.toString → p.typeSignature))
    }.toList

  def extractUnion(tpe: Type): NList[NList[Type]] =
    coproductElements(tpe).collect { case FieldType(KeyName(name), value) ⇒ name -> extractList(value) }

  def extractList(tpe: Type): NList[Type] =
    hlistElements(tpe).collect { case FieldType(KeyName(name), value) ⇒ name -> value }



  class Applier(val input: Type, val impl: Type){
    val inputWiden = input.dealias.widen
    val union = extractUnion(inputWiden)
    val methods = extractMethods(impl)

    def verifyTypes: Boolean = {
      val implMap = methods.iterator.map { case (n, lists) ⇒ n -> lists.iterator.flatMap(_.iterator) }.toMap
      union.foldLeft(true) { case (result, (caseName, inputFields)) ⇒
        implMap.get(caseName) match {
          case None ⇒
            info(s"ERROR: implementation for $caseName is not defined")
            false
          case Some(caseImpl) ⇒
            val inputMap = inputFields.toMap
            caseImpl.foldLeft(result) { case (result2, (paramName, parImplType)) ⇒
              inputMap.get(paramName) match {
                case None ⇒
                  info(s"ERROR parameter $paramName in method $caseName not found in input")
                  false
                case Some(parInputType) if parInputType <:< parImplType ⇒
                  result2
                case Some(parInputType) ⇒
                  info(s"ERROR parameter $paramName in method $caseName has type $parImplType not assignable from $parInputType in input")
                  false
              }
            }
        }
      }
    }

    def output: Type = input


  }

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

  c.freshName()
}

object NamedImplMacros {
  type NList[T] = List[(String, T)]
}

