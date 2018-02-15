package ru.tinkoff.tschema.swagger

import ru.tinkoff.tschema.swagger.SwaggerTyping.{ClassOrTrait, ArgList, Matcher}

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.reflect.ClassTag

class swaggerTyping(circe: Boolean, named: Boolean, name: String) extends StaticAnnotation {
  inline def apply(defn: Any): Any= meta {
    val args = ArgList(this)

    val circe = args.find(Lit.Boolean.unapply _, "circe", 0).getOrElse(false)
    val named = args.find(Lit.Boolean.unapply _, "named", 1).getOrElse(true)
    val name = args.find(Lit.String.unapply _, "name", 2)

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
  implicit class Matcher[A, L <: Lit: ClassTag](f: L => Option[A]){
    def unapply(x: Term.Arg): Option[A] = x match {
      case x: L => f(x)
      case _ => None
    }
  }
  case class ArgList(pos: Vector[Term.Arg], named: Map[String, Term.Arg]) {
    def find[A, L <: Lit : ClassTag](Match: Matcher[A, L], name: String, at: Int): Option[A] =
      (named.get(name) orElse pos.lift(at)).collect { case Match(x) => x }
  }

  object ArgList {
    val empty = new ArgList(Vector.empty, Map.empty)
    def apply(tree: Tree): ArgList =
      tree match {
        case q"new $_(..$lst)" => lst.foldLeft(empty) {
          case (args, Term.Arg.Named(Term.Name(name), v)) => new ArgList(args.pos, args.named + (name -> v))
          case (args, v) => new ArgList(args.pos :+ v, args.named)
        }
        case _ => empty
      }

  }


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

    if (circe) List(typeable, mkCirceEncoder(typeName, derivation), mkCirceDecoder(typeName, derivation))
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
