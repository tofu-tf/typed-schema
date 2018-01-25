package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.swagger.SwaggerTyping.ClassOrTrait

import scala.annotation.StaticAnnotation
import scala.meta._

class swaggerTyping extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(cls @ (ClassOrTrait(name)), companion: Defn.Object)) =>
        val newStats = SwaggerTyping.mkTypeable(name) +: companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls @ (ClassOrTrait(name)) =>
        val companion =
          q""" object ${Term.Name(name.value)}{
             ${SwaggerTyping.mkTypeable(name)}
           }
         """
        Term.Block(List(cls, companion))
      case _ => abort("can decorate only class or traits")
    }
  }
}

object SwaggerTyping {
  object ClassOrTrait{
    def unapply(arg: Defn): Option[Type.Name] = arg match {
      case cls : Defn.Class => Some(cls.name)
      case trt : Defn.Trait=> Some(trt.name)
      case _ => None
    }
  }

  def mkTypeable(name: Type.Name): Stat =
    q"""
       implicit val swaggerTypeable: _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$name] =
          _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable.deriveNamedTypeable
     """
}
