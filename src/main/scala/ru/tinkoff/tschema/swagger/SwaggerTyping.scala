package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.swagger.SwaggerTyping.ClassOrTrait

import scala.annotation.StaticAnnotation
import scala.meta._

class swaggerTyping(circe: Boolean = false, named: Boolean = true, name: Option[String] = None) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val circe = this match {
      case q"new $_(${Lit.Boolean(b)}, ..$_)" => b
      case q"new $_(circe = ${Lit.Boolean(b)}, ..$_)" => b
      case _ => false
    }

    val named = this match {
      case q"new $_(_, ${Lit.Boolean(b)}, ..$_)" => b
      case q"new $_(..$args)" =>
        args.collectFirst {
          case Term.Arg.Named(Term.Name("named"), Lit.Boolean(b)) => b
        }.getOrElse(true)
      case _ => true
    }

    val name = this match {
      case q"new $_(..$_, Some(${Lit.String(n)}))" => Some(n)
      case q"new $_(..$args)" =>
        args.collectFirst {
          case Term.Arg.Named(Term.Name("name"), q"Some(${Lit.String(s)})") => Some(s)
        }.flatten
      case _ => None
    }

    defn match {
      case Term.Block(Seq(cls@(ClassOrTrait(typeName, isCls)), companion: Defn.Object)) =>
        val newStats = SwaggerTyping.mkAll(typeName, circe, isCls, named, name) ++ companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls@(ClassOrTrait(typeName, isCls)) =>
        val companion =
          q""" object ${Term.Name(typeName.value)}{
             ..${SwaggerTyping.mkAll(typeName, circe, isCls, named, name)}
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

  def mkAll(typeName: Type.Name, circe: Boolean, derivation: Boolean, named: Boolean, name: Option[String]): List[Stat] = {
    val typeable = (named, name) match {
      case (true, None) => deriveTypeable(typeName)
      case (true, Some(n)) => mkNamedTypeable(typeName, Lit.String(n))
      case (false, _) => mkTypeable(typeName)
    }

    if(circe) List(typeable, mkCirceEncoder(typeName, derivation), mkCirceDecoder(typeName, derivation))
    else List(typeable)
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
