package ru.tinkoff.tschema.utils

import scala.meta._
import scala.reflect.ClassTag


object MacroUtils{
  implicit class Matcher[A, L <: Lit : ClassTag](f: L => Option[A]) {
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
}
