package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.utils.MacroUtils._

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.reflect.ClassTag

class SwaggerTyping(named: Boolean = true, name: String = "", magnolia: Boolean = false) extends StaticAnnotation {
  inline def apply(defn: Any): Any
  = meta {
    val args = ArgList(this)

    val named = args.find(Lit.Boolean.unapply _, "named", 1).getOrElse(true)
    val name = args.find(Lit.String.unapply _, "name", 2)
    val magnolia = args.find(Lit.Boolean.unapply _, "magnolia", 3).getOrElse(false)

    defn match {
      case Term.Block(Seq(cls@(ClassOrTrait(typeName, isCls)), companion: Defn.Object)) =>
        val newStats = SwaggerTyping.mkAll(typeName, isCls, named, name, magnolia) ++ companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls@(ClassOrTrait(typeName, isCls)) =>
        val companion =
          q""" object ${Term.Name(typeName.value)}{
             ..${SwaggerTyping.mkAll(typeName, isCls, named, name, magnolia)}
           }
         """
        Term.Block(List(cls, companion))
      case _ => abort("can decorate only class or traits")
    }
  }
}

object SwaggerTyping {
  trait Strategy {
    def default: Term
    def renamed: Term
    def anon: Term
  }

  def mkAll(typeName: Type.Name, derivation: Boolean, named: Boolean, name: Option[String], magnolia: Boolean): List[Stat] = {

    def typ: Type = t"_root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$typeName]"

    def newName = name.map(Lit.String(_)).getOrElse(abort("should not happend"))

    def simpleStrat = new Strategy {
      def module: Term = q"_root_.ru.tinkoff.tschema.swagger.SwaggerTypeable"
      def default: Term = q"$module.deriveNamedTypeable[$typeName]"
      def renamed: Term = q"$module.genNamedTypeable[$typeName]($newName)"
      def anon: Term = q"$module.genTypeable[$typeName]"
    }

    def magnoliaStrat = new Strategy {
      def module: Term = q"_root_.ru.tinkoff.tschema.swagger.MagnoliaSwagger"
      override def default: Term = q"$module.derive[$typeName]"
      override def anon: Term = q"$module.derive[$typeName].anon"
      override def renamed: Term = q"$module.derive[$typeName].named($newName)"
    }

    val strat = if (magnolia) magnoliaStrat else simpleStrat

    val rhs = (named, name) match {
      case (true, None) => strat.default
      case (true, Some(n)) => strat.renamed
      case (false, _) => strat.anon
    }

    val typeable = q"implicit lazy val swaggerTypeable: $typ = $rhs"

    List(typeable)
  }
}
