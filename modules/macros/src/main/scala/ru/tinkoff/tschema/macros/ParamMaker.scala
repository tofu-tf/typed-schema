package ru.tinkoff.tschema.macros
import ru.tinkoff.tschema.macros.ParamMaker.Applyer
import ru.tinkoff.tschema.typeDSL
import ru.tinkoff.tschema.typeDSL.{Capture, DSLAtom, FormField, Header, QueryParam, MultipartField}

import scala.language.{dynamics, higherKinds}
import scala.language.experimental.macros
import shapeless.Witness

import scala.reflect.macros.whitebox

class ParamMaker[T[_, _]] extends Dynamic {
  def params[A]: Applyer = macro ParamMakerMacro.paramsImpl[A, T[_, _]]
}

object ParamMaker {
  type Applyer = { def apply[A](x: => A): DSLAtom }
  def apply[T[_, _]]: ParamMaker[T] = new ParamMaker[T]

  object query          extends ParamMaker[QueryParam]
  object path           extends ParamMaker[Capture]
  object headers        extends ParamMaker[Header]
  object form           extends ParamMaker[FormField]
  object multipartField extends ParamMaker[MultipartField]
}

class ParamMakerMacro(val c: whitebox.Context) extends SymbolMacros {
  import c.universe._

  val consTpe = typeOf[typeDSL.:>[_, _]].typeConstructor

  def paramsImpl[A: WeakTypeTag, T: WeakTypeTag] = {
    val const = weakTypeTag[T].tpe.typeConstructor

    val result = extractNamesTypes(weakTypeOf[A]).map { case (name, tpe) => appliedType(const, KeyName(name), tpe) }
      .reduce((a, b) => appliedType(consTpe, a, b))

    q"""
    new {
      import ru.tinkoff.tschema.typeDSL.{:>}
      def apply[A](x: => A): :>[$result, A] = new :>
    }"""
  }

  def extractNamesTypes(ref: Type): List[(String, Type)] = ref match {
    case RefinedType(tpes, scope) =>
      info(tpes)
      scope.collect { case m: MethodSymbol =>
        info(m)
        m.name.decodedName.toString -> m.returnType
      }.toList
    case _                        => List.empty
  }

  def info[A](mess: A) = c.info(c.enclosingPosition, mess.toString, force = false)
}
