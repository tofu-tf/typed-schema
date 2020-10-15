package ru.tinkoff.tschema.macros

import java.time.Instant

import cats.kernel.Monoid
import cats.syntax.monoid._
import cats.syntax.option._
import ru.tinkoff.tschema.typeDSL._
import shapeless.{HList, HNil}

import scala.language.experimental.macros
import scala.reflect.NameTransformer
import scala.reflect.macros.{TypecheckException, blackbox}

class MakerMacro(val c: blackbox.Context) extends ShapelessMacros with SingletonMacros {

  import c.universe._
  type WTTF[F[_]] = WeakTypeTag[F[Unit]]

  def makeRouteHNilNoImpl[F[_]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Res: WeakTypeTag](
      definition: c.Expr[Def]
  ): c.Expr[Res] =
    makeRouteImpl[F, If, Def, Unit, Res, HNil](definition)(none)(c.Expr(q"()"))

  def makeRouteHNil[F[_]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
      definition: c.Expr[Def]
  )(
      impl: c.Expr[Impl]
  ): c.Expr[Res] =
    makeRouteImpl[F, If, Def, Impl, Res, HNil](definition)(impl.tree.some)(c.Expr(q"(HNil : HNil)"))

  def makeRoute[F[_]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag, In: WeakTypeTag](
      definition: c.Expr[Def]
  )(
      impl: c.Expr[Impl]
  )(
      input: c.Expr[In]
  ): c.Expr[Res] = makeRouteImpl[F, If, Def, Impl, Res, In](definition)(impl.tree.some)(input)

  private def makeRouteImpl[F[
      _
  ]: WTTF, If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag, In: WeakTypeTag](
      definition: c.Expr[Def]
  )(
      impl: Option[Tree]
  )(
      input: c.Expr[In]
  ): c.Expr[Res] = {
    val ifP       = getPackage(weakTypeOf[If])
    val defT      = weakTypeOf[Def]
    val implT     = weakTypeOf[Impl]
    val ft        = weakTypeOf[F[Unit]].typeConstructor
    val dsl       = constructDslTree(defT, Monoid.empty[PrefixInfo[Type]])
    val wholeTree = new RouteTreeMaker(impl).makeRouteTree(ft, dsl, input.tree)

    infoTime(s"route $implT")

    val tree =
      q"""
        {
          val $interface =  $ifP
          import shapeless.{::, HNil}

          $wholeTree
        }
        """

    c.Expr(typeCheckOrAbort(tree))
  }

  def makeResult[F[_]: WTTF, In <: HList: WeakTypeTag, Out: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
      in: c.Expr[In]
  )(
      impl: c.Expr[Impl]
  )(
      key: c.Expr[String],
      groups: c.Expr[String]*
  ): c.Expr[Res] = {
    val implT      = weakTypeOf[Impl].dealias
    val inT        = weakTypeOf[In].dealias
    val outT       = weakTypeOf[Out].dealias
    val ft         = weakTypeOf[F[Unit]].typeConstructor
    val methName   = unpackString("key")(key)
    val groupNames = groups.toVector.map(unpackString("group"))
    val fullKey    = (groupNames :+ methName).mkString

    val meth = findMeth(implT, groupNames, TermName(methName)) getOrElse abort(s"method $fullKey is not implemented")

    val rec  = extractRecord(inT)
    val syms = rec.flatten.map { case (paramName, _) => paramName -> freshName(paramName) }.toMap
    infoTime(s"result $outT $meth $implT")

    val params = meth.paramLists.map(_.map { p =>
      val name = symbolName(p)
      syms.getOrElse(name, abort(s"could not find input for parameter $name of method $fullKey "))
    })

    val recpat = rec.foldRight(pq"_": Tree) {
      case (None, pat)            => pq"_ :: $pat"
      case (Some((name, _)), pat) => pq"(${syms(name)} @ _) :: $pat"
    }

    val accessor = groupNames.map(TermName(_)).foldLeft[Tree](impl.tree)((a, gr) => q"$a.$gr")

    c.Expr(typeCheckOrAbort(q""" $in match { case $recpat =>
        def res = $accessor.$meth(...$params)
        val route = $interface.route(res)
        route[$ft, $inT, $outT]($in)}"""))
  }

  val ConsC     = typeOf[:>[_, _]].typeConstructor
  val SplitC    = typeOf[<|>[_, _]].typeConstructor
  val dslAtom   = typeOf[DSLAtom]
  val CompleteC = typeOf[Complete[_]].typeConstructor
  val keyC      = typeOf[Key[_]].typeConstructor
  val groupC    = typeOf[Group[_]].typeConstructor
  val interface = TermName("macroInterface")

  private class RouteTreeMaker(impl: Option[Tree]) {
    type DSL = DSLTree[Type]
    def makeRouteTree(ft: Type, dsl: DSL, input: Tree): Tree = dsl match {
      case DSLLeaf(resTyp, groups, key) =>
        impl match {
          case None       => q"""{
              $interface.makeResult[$ft, $resTyp]($input)($key, ..$groups)
             }"""
          case Some(impl) => q"""{
            $interface.makeResult[$ft, $resTyp]($input)($impl)($key, ..$groups)
          }"""
        }

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

  private def extractMeth(typ: Type, name: Name): Option[MethodSymbol] =
    typ.decl(name) match {
      case ms: MethodSymbol                 => Some(ms)
      case ov if ov.alternatives.length > 1 => abort("could not handle method overloading")
      case _                                =>
        typ.baseClasses.tail.iterator.collect {
          case base: TypeSymbol if base != typ => base.toType
        }.flatMap {
          extractMeth(_, name)
        }.collectFirst { case x =>
          x
        }
    }

  private def findMeth(typ: Type, group: Vector[String], name: Name): Option[MethodSymbol] =
    group match {
      case first +: rest =>
        typ.decl(TermName(first)) match {
          case ms: MethodSymbol if ms.paramLists == Nil => findMeth(ms.typeSignature, rest, name)
          case ms: MethodSymbol                         => abort(s"group $first is a method with parameters : ${ms.paramLists}")
          case ms: ModuleSymbol                         => findMeth(ms.typeSignature, rest, name)
          case _                                        => None
        }
      case Vector()      => extractMeth(typ, name)
    }

  private def unpackString(name: String)(sExpr: c.Expr[String]): String =
    sExpr.tree match {
      case Literal(Constant(s: String)) => NameTransformer.encode(s)
      case x                            => abort(s"inproper use of `makeResult` $name should be a string constant, got $x")
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
  object Group    extends CombMatcher(groupC)
  object Complete extends CombMatcher(CompleteC)

  def constructDefPrefix(t: Type): PrefixInfo[Type] = t match {
    case Key(KeyName(key))   => PrefixInfo(Some(key), Vector(), Vector(t))
    case Group(KeyName(grp)) => PrefixInfo(None, Vector(grp), Vector(t))
    case _ if t <:< dslAtom  => PrefixInfo(None, Vector(), Vector(t))
    case Cons(x, y)          => constructDefPrefix(x) |+| constructDefPrefix(y)
    case Split(t1, t2)       => abort(s"split over $t1 and $t2 in prefix: this is not allowed")
    case Complete(_)         => abort(s"$t is usable only as tail element in the DSL")
    case t                   => abort(s"can't use type $t in the DSL,make sure your atom is subtype of DslAtom")
  }

  def constructDslTree(t: Type, prefix: PrefixInfo[Type]): DSLTree[Type] = t match {
    case Complete(res) =>
      prefix.key match {
        case Some(str) => DSLLeaf(res, prefix.groups, str)
        case None      =>
          val typLine = (prefix.prefix :+ t).map(showType).mkString(" :> ")
          abort(s"method key for $typLine is not defined")
      }

    case Cons(x, y) =>
      val px = constructDefPrefix(x)
      constructDslTree(y, prefix |+| px) match {
        case DSLBranch(p2, cdn) => DSLBranch(px.prefix ++ p2, cdn)
        case res                => DSLBranch(px.prefix, Vector(res))
      }

    case Split(x, y) =>
      val t1 = constructDslTree(x, prefix)
      constructDslTree(y, prefix) match {
        case DSLBranch(Vector(), cdn) => DSLBranch(Vector(), t1 +: cdn)
        case t2                       => DSLBranch(Vector(), Vector(t1, t2))
      }

    case _ if t <:< dslAtom => abort(s"could not end DSL with $t")
    case _                  => abort(s"type $t is not a part of type DSL")
  }

  def extractMethod(meth: MethodSymbol): MethodDecl[Type] =
    meth.paramLists.map(lst => lst.map(p => symbolName(p) -> p.typeSignature)) -> meth.returnType

  def symbolName(symbol: Symbol)                          = symbol.name.decodedName.toString

  def makeRouteTree(t: Type): Tree = {
    val defTree = constructDslTree(t, Monoid.empty[PrefixInfo[Type]])

    EmptyTree
  }

  def showType(t: Type): String = t.dealias match {
    case SingletonTypeStr(s)              => s
    case TypeRef(_, s, Nil)               => symbolName(s)
    case TypeRef(_, s, xs) if xs.nonEmpty => xs.map(showType).mkString(s"${symbolName(s)}[", ",", "]")
    case t1                               => t1.toString()
  }

  def getPackage(t: Type): Tree       =
    t.typeSymbol.fullName
      .split("\\.")
      .foldLeft[Tree](q"_root_") { (pack, name) =>
        q"$pack.${TermName(name)}"
      }
  private def infoTime(label: String) = if (false) info(s"$label: ${Instant.now().toString}")

  private def typeCheckOrAbort(t: Tree): Tree =
    try (c.typecheck(t))
    catch {
      case ex: TypecheckException => c.abort(c.enclosingPosition, ex.toString)
    }
}
