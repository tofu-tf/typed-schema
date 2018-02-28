package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.utils.MacroUtils._

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.reflect.ClassTag

class swaggerTyping(named: Boolean = true, name: String = "") extends StaticAnnotation {
  inline def apply(defn: Any): Any
  = meta {
    val args = ArgList(this)

    val named = args.find(Lit.Boolean.unapply _, "named", 1).getOrElse(true)
    val name = args.find(Lit.String.unapply _, "name", 2)

    defn match {
      case Term.Block(Seq(cls@(ClassOrTrait(typeName, isCls)), companion: Defn.Object)) =>
        val newStats = SwaggerTyping.mkAll(typeName, isCls, named, name) ++ companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls@(ClassOrTrait(typeName, isCls)) =>
        val companion =
          q""" object ${Term.Name(typeName.value)}{
             ..${SwaggerTyping.mkAll(typeName, isCls, named, name)}
           }
         """
        Term.Block(List(cls, companion))
      case _ => abort("can decorate only class or traits")
    }
  }
}

object SwaggerTyping {
  def mkAll(typeName: Type.Name, derivation: Boolean, named: Boolean, name: Option[String]): List[Stat] = {
    val typeable = (named, name) match {
      case (true, None) => deriveTypeable(typeName)
      case (true, Some(n)) => mkNamedTypeable(typeName, Lit.String(n))
      case (false, _) => mkTypeable(typeName)
    }

    List(typeable)
  }

  def deriveTypeable(typeName: Type.Name): Stat =
    q"""
       implicit val swaggerTypeable: _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$typeName] =
          _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable.deriveNamedTypeable
     """

  def mkNamedTypeable(typeName: Type.Name, name: Lit.String): Stat =
    q"""
      implicit val swaggerTypeable: _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$typeName] =
        _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable.genNamedTypeable[$typeName]($name)
    """

  def mkTypeable(typeName: Type.Name): Stat =
    q"""
       implicit val swaggerTypeable: _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$typeName] =
          _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable.genTypeable
     """
}
