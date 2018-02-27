package ru.tinkoff.tschema.utils.json

import CirceCodec._

import scala.annotation.StaticAnnotation
import scala.meta._
import ru.tinkoff.tschema.utils.MacroUtils._

class circeCodec(derivation: Boolean = true, snake: Boolean = false) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val args = ArgList(this)

    val derivation = args.find(Lit.Boolean.unapply _, "derivation", 0).getOrElse(true)
    val snake = args.find(Lit.Boolean.unapply _, "snake", 1).getOrElse(false)

    defn match {
      case Term.Block(Seq(cls@(ClassOrTrait(typeName, isCls)), companion: Defn.Object)) =>
        val newStats = mkAll(typeName, derivation, snake) ++ companion.templ.stats.toList.flatten
        val newComp = companion.copy(templ = companion.templ.copy(stats = Some(newStats)))
        Term.Block(List(cls, newComp))
      case cls@(ClassOrTrait(typeName, isCls)) =>
        val companion =
          q""" object ${Term.Name(typeName.value)}{
             ..${mkAll(typeName, derivation, snake)}
           }
         """
        Term.Block(List(cls, companion))
      case _ => abort("can decorate only class or traits")
    }
  }
}

object CirceCodec {
  val snakeCase = q"_root_.io.circe.derivation.renaming.snakeCase"

  def mkAll(typeName: Type.Name, derivation: Boolean, snake: Boolean): List[Stat] = {
    def mkCirceEncoder: Stat = {
      val call = if (derivation) {
        if (snake) q"_root_.io.circe.derivation.deriveEncoder($snakeCase)"
        else q"_root_.io.circe.derivation.deriveEncoder"
      }
      else q"_root_.io.circe.generic.extras.semiauto.deriveEncoder"
      q"implicit val encoder: _root_.io.circe.Encoder[$typeName] = $call"
    }

    def mkCirceDecoder: Stat = {
      val call = if (derivation) {
        if (snake) q"_root_.io.circe.derivation.deriveDecoder($snakeCase)" else
          q"_root_.io.circe.derivation.deriveDecoder"
      }
      else q"_root_.io.circe.generic.extras.semiauto.deriveDecoder"
      q"implicit val decoder: _root_.io.circe.Decoder[$typeName] = $call"
    }

    List(mkCirceEncoder, mkCirceDecoder)
  }
}
