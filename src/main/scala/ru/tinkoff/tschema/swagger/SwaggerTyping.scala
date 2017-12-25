package ru.tinkoff.tschema.swagger

import scala.annotation.StaticAnnotation
import scala.meta._

class swaggerTyping extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(cls: Defn.Class, companion: Defn.Object)) =>
        val newStats = SwaggerTyping.mkTypeable(cls.name) +: companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls: Defn.Class =>
        val companion =
          q""" object ${Term.Name(cls.name.value)}{
             ${SwaggerTyping.mkTypeable(cls.name)}
           }
         """
        Term.Block(List(cls, companion))
    }
  }
}

object SwaggerTyping {
  def mkTypeable(name: Type.Name): Stat =
    q"""
       implicit val swaggerTypeable: _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$name] =
          _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable.deriveNamedTypeable
     """
}
