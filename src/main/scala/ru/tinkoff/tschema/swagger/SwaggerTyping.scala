package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.swagger.SwaggerTyping.ClassOrTrait

import scala.annotation.StaticAnnotation
import scala.meta._

class swaggerTyping(circe: Boolean = false) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val circe = this match {
      case q"new $_(${Lit.Boolean(b)})" => b
      case q"new $_(circe = ${Lit.Boolean(b)})" => b
      case _ => false
    }

    defn match {
      case Term.Block(Seq(cls@(ClassOrTrait(name, isCls)), companion: Defn.Object)) =>
        val newStats = SwaggerTyping.mkAll(name, circe, isCls) ++ companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls@(ClassOrTrait(name, isCls)) =>
        val companion =
          q""" object ${Term.Name(name.value)}{
             ..${SwaggerTyping.mkAll(name, circe, isCls)}
           }
         """
        Term.Block(List(cls, companion))
      case _ => abort("can decorate only class or traits")
    }
  }
}

object SwaggerTyping {

  object ClassOrTrait {
    def unapply(arg: Defn): Option[(Type.Name, Boolean)] = arg match {
      case cls: Defn.Class => Some((cls.name, true))
      case trt: Defn.Trait => Some((trt.name, false))
      case _ => None
    }
  }

  def mkAll(name: Type.Name, circe: Boolean, derivation: Boolean): List[Stat] =
    if(circe) List(mkTypeable(name), mkCirceEncoder(name, derivation), mkCirceDecoder(name, derivation))
    else List(mkTypeable(name))

  def mkTypeable(name: Type.Name): Stat =
    q"""
       implicit val swaggerTypeable: _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable[$name] =
          _root_.ru.tinkoff.tschema.swagger.SwaggerTypeable.deriveNamedTypeable
     """

  def mkCirceEncoder(name: Type.Name, derivation: Boolean): Stat = {
    val call = if (derivation) q"_root_.io.circe.derivation.deriveEncoder"
    else q"_root_.io.circe.generic.extras.semiauto.deriveEncoder"
    q"implicit val encoder: _root_.io.circe.Encoder[$name] = $call"
  }

  def mkCirceDecoder(name: Type.Name, derivation: Boolean): Stat = {
    val call = if (derivation) q"_root_.io.circe.derivation.deriveDecoder"
    else q"_root_.io.circe.generic.extras.semiauto.deriveDecoder"
    q"implicit val decoder: _root_.io.circe.Decoder[$name] = $call"
  }
}
