package ru.tinkoff.tschema.macros

import java.time.Instant

import MakerMacro._
import shapeless.HNil
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.TypeCheckError
import ru.tinkoff.tschema.macros.{ShapelessMacros, SingletonMacros}
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, HNil}

import scala.language.experimental.macros
import scala.reflect.NameTransformer
import scala.reflect.macros.{TypecheckException, blackbox}

class MakerMacro(val c: blackbox.Context) extends ShapelessMacros with SingletonMacros {

  import c.universe._
  type WTTF[F[_]] = WeakTypeTag[F[Unit]]

  def makeRouteHNilUnit[F[_]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Res: WeakTypeTag](
      definition: c.Expr[Def]): c.Expr[Res] =
    makeRouteHNil[F, If, Def, Unit, Res](definition)(c.Expr(q"()"))

  def makeRouteHNil[F[_]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
      definition: c.Expr[Def]
  )(
      impl: c.Expr[Impl]
  ): c.Expr[Res] =
    makeRoute[F, If, Def, Impl, Res, HNil](definition)(impl)(c.Expr(q"(HNil : HNil)"))

  def makeRoute[F[_]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag, In: WeakTypeTag](
      definition: c.Expr[Def]
  )(
      impl: c.Expr[Impl]
  )(
      input: c.Expr[In]
  ): c.Expr[Res] = {
    val ifP       = getPackage(weakTypeOf[If])
    val defT      = weakTypeOf[Def]
    val implT     = weakTypeOf[Impl]
    val ft        = weakTypeOf[F[Unit]].typeConstructor
    val dsl       = constructDslTree(defT)
    val wholeTree = new RouteTreeMaker(impl.tree).makeRouteTree(ft, dsl, input.tree)

    infoTime(s"route $implT")

    val tree =
      q"""
        {
          val $interface =  $ifP
          import shapeless.{::, HNil}

          $wholeTree
        }
        """

    val checkedTree = try {
      c.typecheck(tree)
    } catch {
      case typecheck: TypecheckException => abort(typecheck.getMessage)
    }

    c.Expr(checkedTree)
  }

  def makeResult[F[_]: WTTF, In <: HList: WeakTypeTag, Out: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
      in: c.Expr[In]
  )(
      impl: c.Expr[Impl]
  )(
      key: c.Expr[String]
  ): c.Expr[Res] = {
    val implT = weakTypeOf[Impl].dealias
    val inT   = weakTypeOf[In].dealias
    val outT  = weakTypeOf[Out].dealias
    val ft    = weakTypeOf[F[Unit]].typeConstructor
    val keyS: String = key.tree match {
      case Literal(Constant(s: String)) => NameTransformer.encode(s)
      case _                            => abort("inproper use of `makeResult` key should be a string constant")
    }
    val meth = findMeth(implT, TermName(keyS)) getOrElse abort(s"method $keyS is not implemented")

    val rec  = extractRecord(inT)
    val syms = rec.flatten.map { case (paramName, _) => paramName -> freshName(paramName) }.toMap
    infoTime(s"result $outT $meth $implT")

    val params = meth.paramLists.map(_.map { p =>
      val name = symbolName(p)
      syms.getOrElse(name, abort(s"could not find input for parameter $name of method $keyS "))
    })
    val recpat = rec.foldRight(pq"_": Tree) {
      case (None, pat)            => pq"_ :: $pat"
      case (Some((name, _)), pat) => pq"(${syms(name)} @ _) :: $pat"
    }

    c.Expr(c.typecheck(q""" $in match { case $recpat =>
        def res = $impl.$meth(...$params)
        val route = $interface.route(res)
        route[$ft, $inT, $outT]($in)}"""))
  }

  val ConsC     = typeOf[:>[_, _]].typeConstructor
  val SplitC    = typeOf[<|>[_, _]].typeConstructor
  val dslAtom   = typeOf[DSLAtom]
  val CompleteC = typeOf[Complete[_]].typeConstructor
  val keyC      = typeOf[Key[_]].typeConstructor
  val interface = TermName("macroInterface")

  private class RouteTreeMaker(impl: Tree) {
    type DSL = DSLTree[Type]
    def makeRouteTree(ft: Type,  dsl: DSL, input: Tree): Tree = dsl match {
      case DSLLeaf(resTyp, key) =>
        q"""{
          $interface.makeResult[$ft, $resTyp]($input)($impl)($key)
          }"""
      case DSLBranch(pref +: next, dsls) =>
        val identName = freshName("input")
        val tpt       = tq""
        val ident     = q"val $identName: $tpt"
        val rest      = makeRouteTree(ft, DSLBranch(next, dsls), Ident(identName))
        q"""$interface.serve[$ft, $pref]($input).apply(($ident) => $rest)"""
      case DSLBranch(_, dsls) => makeRouteSumTree(ft, dsls, input)
    }

    def makeRouteSumTree(ft: Type, dsls: Vector[DSL], input: Tree): Tree =
      dsls.map(makeRouteTree(ft, _, input)).reduce { (l, r) =>
        q"""$interface.concatResults($l,$r)"""
      }
  }

  private def findMeth(typ: Type, name: Name): Option[MethodSymbol] =
    typ.decl(name) match {
      case ms: MethodSymbol                 => Some(ms)
      case ov if ov.alternatives.length > 1 => abort("could not handle method overloading")
      case _ =>
        typ.baseClasses.tail.iterator.collect {
          case base: TypeSymbol if base != typ => base.toType
        }.flatMap {
          findMeth(_, name)
        }.collectFirst { case x => x }
    }

  class CombMatcher(constr: Type) {
    def unapplySeq(t: Type): Option[List[Type]] = t baseType constr.typeSymbol match {
      case TypeRef(_, sym, xs) if sym.asType.toType.typeConstructor =:= constr => Some(xs)
      case _                                                                   => None
    }
  }

  object Cons     extends CombMatcher(ConsC)
  object Split    extends CombMatcher(SplitC)
  object Key      extends CombMatcher(keyC)
  object Complete extends CombMatcher(CompleteC)

  def constructDefPrefix(t: Type): (Vector[Type], Option[String]) = t match {
    case Key(KeyName(key))  => (Vector(t), Some(key))
    case _ if t <:< dslAtom => (Vector(t), None)
    case Cons(x, y) =>
      val (a, k1) = constructDefPrefix(x)
      val (b, k2) = constructDefPrefix(y)
      (a ++ b, k1 orElse k2)
    case Split(t1, t2) => abort(s"split over $t1 and $t2 in prefix: this is not allowed")
    case Complete(_)   => abort(s"$t is usable only as tail element in the DSL")
  }

  def constructDslTree(t: Type, key: Option[String] = None, prefix: Vector[Type] = Vector.empty): DSLTree[Type] = t match {
    case Complete(res) =>
      key match {
        case Some(str) => DSLLeaf(res, str)
        case None =>
          val typLine = (prefix :+ t).map(showType).mkString(" :> ")
          abort(s"method key for $typLine is not defined")
      }

    case Cons(x, y) =>
      val (p1, k1) = constructDefPrefix(x)
      constructDslTree(y, key orElse k1, prefix = prefix ++ p1) match {
        case DSLBranch(p2, cdn) => DSLBranch(p1 ++ p2, cdn)
        case res                => DSLBranch(p1, Vector(res))
      }

    case Split(x, y) =>
      val t1 = constructDslTree(x, key, prefix)
      constructDslTree(y, key, prefix) match {
        case DSLBranch(Vector(), cdn) => DSLBranch(Vector(), t1 +: cdn)
        case t2                       => DSLBranch(Vector(), Vector(t1, t2))
      }

    case _ if t <:< dslAtom => abort(s"could not end DSL with $t")
    case _                  => abort(s"type $t is not a part of type DSL")
  }

  def extractMethod(meth: MethodSymbol): MethodDecl[Type] =
    meth.paramLists.map(lst => lst.map(p => symbolName(p) -> p.typeSignature)) -> meth.returnType

  def symbolName(symbol: Symbol) = symbol.name.decodedName.toString

  def makeRouteTree(t: Type): Tree = {
    val defTree = constructDslTree(t)

    EmptyTree
  }

  def showType(t: Type): String = t.dealias match {
    case SingletonTypeStr(s)              => s
    case TypeRef(_, s, Nil)               => symbolName(s)
    case TypeRef(_, s, xs) if xs.nonEmpty => xs.map(showType).mkString(s"${symbolName(s)}[", ",", "]")
  }

  def getPackage(t: Type): Tree =
    t.typeSymbol.fullName
      .split("\\.")
      .foldLeft[Tree](q"_root_") { (pack, name) =>
        q"$pack.${TermName(name)}"
      }
  private def infoTime(label: String) = if (false) info(s"$label: ${Instant.now().toString}")
}

object MakerMacro {
  sealed trait DSLTree[T]
  case class DSLBranch[T](pref: Vector[T], children: Vector[DSLTree[T]]) extends DSLTree[T]
  case class DSLLeaf[T](res: T, key: String)                             extends DSLTree[T]

  type Skip[A] = Any

  type NList[T]      = List[(String, T)]
  type MethodDecl[T] = (List[NList[T]], T)
}
