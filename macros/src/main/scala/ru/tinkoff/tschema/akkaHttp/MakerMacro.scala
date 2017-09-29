package ru.tinkoff.tschema.akkaHttp

import akka.http.scaladsl.server.Route
import ru.tinkoff.tschema.macros.{MacroMessages, ShapelessMacros, SingletonMacros, SymbolMacros}
import ru.tinkoff.tschema.typeDSL._
import shapeless.{CaseClassMacros, HList}

import language.experimental.macros
import scala.reflect.macros.blackbox
import MakerMacro._



class MakerMacro(val c: blackbox.Context) extends ShapelessMacros with SingletonMacros {

  import c.universe._

  val ConsC = typeOf[:>[_, _]].typeConstructor
  val SplitC = typeOf[<|>[_, _]].typeConstructor
  val dslAtom = typeOf[DSLAtom]
  val dslRes = typeOf[DSLRes]
  val keyC = typeOf[Key[_]].typeConstructor

  class RouteTreeMaker(impl: Tree) {
    type DSL = DSLTree[Type]
    def makeRouteTree(dsl: DSL, input: Tree): Tree = dsl match {
      case DSLLeaf(resTyp, key) => q"makeResult[$resTyp].apply($input)($impl)($key)"
      case DSLBranch(pref +: next, dsls) =>
        val ident = freshName("input")
        val rest = makeRouteTree(DSLBranch(next, dsls), Ident(ident))
        q"""$input.serve[$pref].tapply{
               case Tuple1($ident) => $rest
             }"""
      case DSLBranch(_, dsls) => makeRouteSumTree(dsls, input)
    }

    def makeRouteSumTree(dsls: Vector[DSL], input: Tree): Tree =
      dsls.map(makeRouteTree(_, input)).reduce { (l, r) =>
        q"""concatResults($l,$r)"""
      }
  }

  def makeRoute[If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag](definition: Tree)(impl: Tree): Tree = {
    val ifP = getPackage(weakTypeOf[If])
    val defT = weakTypeOf[Def]
    val implT = weakTypeOf[Impl]
    val dsl = constructDslTree(defT)
    val startInput = q"(HNil : HNil)"
    val wholeTree = new RouteTreeMaker(impl).makeRouteTree(dsl, startInput)

    val tree =
      q"""
       {
        import $ifP._
        import shapeless.{::, HNil}

        $wholeTree
       }
     """
    tree
  }

  def makeResult[In <: HList : WeakTypeTag, Out: WeakTypeTag, Impl: WeakTypeTag](in: Tree)(impl: Tree)(key: Tree): Tree = {
    val implT = weakTypeOf[Impl].dealias
    val inT = weakTypeOf[In].dealias
    val outT = weakTypeOf[Out].dealias
    val keyS: String = key match {
      case Literal(Constant(s: String)) => s
      case _ => abort("inproper use of `makeResult` key should be a string constant")
    }
    val meth = implT.decl(TermName(keyS)) match {
      case ms: MethodSymbol => ms
      case _ => abort(s"method $keyS is not implemented")
    }

    val rec = extractRecord(inT)
    val syms = rec.flatten.map { case (paramName, _) => paramName -> freshName(paramName) }.toMap
    val params = meth.paramLists.map(_.map { p =>
      val name = symbolName(p)
      syms.getOrElse(name, abort(s"could not find input for parameter $name of method $keyS "))
    })
    val recpat = rec.foldRight(pq"_": Tree) {
      case (None, pat) => pq"_ :: $pat"
      case (Some((name, _)), pat) => pq"(${syms(name)} @ _) :: $pat"
    }

    q""" $in match {
       case $recpat =>  $impl.$meth(...$params).route[$outT]
       }"""
  }

  class CombMatcher(constr: Type) {
    def unapplySeq(t: Type): Option[List[Type]] = t baseType constr.typeSymbol match {
      case TypeRef(_, sym, xs) if sym.asType.toType.typeConstructor =:= constr => Some(xs)
      case _ => None
    }
  }

  object Cons extends CombMatcher(ConsC)
  object Split extends CombMatcher(SplitC)
  object Key extends CombMatcher(keyC)

  def constructDefPrefix(t: Type): (Vector[Type], Option[String]) = t match {
    case Key(KeyName(key)) => (Vector(t), Some(key))
    case _ if t <:< dslAtom => (Vector(t), None)
    case Cons(x, y) =>
      val (a, k1) = constructDefPrefix(x)
      val (b, k2) = constructDefPrefix(y)
      (a ++ b, k1 orElse k2)
    case _ if t <:< dslRes => abort(s"$t is usable only as tail element in the DSL")
  }

  def constructDslTree(t: Type, key: Option[String] = None, prefix: Vector[Type] = Vector.empty): DSLTree[Type] = t match {
    case _ if t <:< dslRes =>
      key match {
        case Some(str) => DSLLeaf(t, str)
        case None =>
          val typLine = (prefix :+ t).map(showType).mkString(" :> ")
          abort(s"method key for $typLine is not defined")
      }

    case Cons(x, y) =>
      val (p1, k1) = constructDefPrefix(x)
      constructDslTree(y, key orElse k1, prefix = prefix ++ p1) match {
        case DSLBranch(p2, cdn) => DSLBranch(p1 ++ p2, cdn)
        case res => DSLBranch(p1, Vector(res))
      }

    case Split(x, y) =>
      val t1 = constructDslTree(x, key, prefix)
      constructDslTree(y, key, prefix) match {
        case DSLBranch(Vector(), cdn) => DSLBranch(Vector(), t1 +: cdn)
        case t2 => DSLBranch(Vector(), Vector(t1, t2))
      }

    case _ if t <:< dslAtom => abort(s"could not end DSL with $t")
    case _ => abort(s"type $t is not a part of type DSL")
  }

  def extractMethod(meth: MethodSymbol): MethodDecl[Type] =
    meth.paramLists.map(lst => lst.map(p => symbolName(p) -> p.typeSignature)) -> meth.returnType

  def symbolName(symbol: Symbol) = symbol.name.decodedName.toString

  def makeRouteTree(t: Type): Tree = {
    val defTree = constructDslTree(t)

    EmptyTree
  }

  def showType(t: Type): String = t.dealias match {
    case SingletonTypeStr(s) => s
    case TypeRef(_, s, Nil) => symbolName(s)
    case TypeRef(_, s, xs) if xs.nonEmpty => xs.map(showType).mkString(s"${symbolName(s)}[", ",", "]")
  }

  def getPackage(t: Type): Tree =
    t.typeSymbol.fullName.split("\\.")
    .foldLeft[Tree](q"_root_") { (pack, name) => q"$pack.${TermName(name)}" }


}

object MakerMacro {
  sealed trait DSLTree[T]
  case class DSLBranch[T](pref: Vector[T], children: Vector[DSLTree[T]]) extends DSLTree[T]
  case class DSLLeaf[T](res: T, key: String) extends DSLTree[T]

  type NList[T] = List[(String, T)]
  type MethodDecl[T] = (List[NList[T]], T)
}




