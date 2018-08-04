package ru.tinkoff.tschema.akkaHttp

import java.time.Instant

import MakerMacro._
import cats.data.{NonEmptyList, OptionT}
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.TypeCheckError
import ru.tinkoff.tschema.macros.{ShapelessMacros, SingletonMacros}
import ru.tinkoff.tschema.typeDSL._
import shapeless.HList
import cats.syntax.either._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.instances.list._
import cats.instances.either._
import cats.syntax.alternative._

import language.experimental.macros
import scala.reflect.NameTransformer
import scala.reflect.macros.{TypecheckException, blackbox}

class MakerMacro(val c: blackbox.Context) extends ShapelessMacros with SingletonMacros {

  type Result[A] = Either[String, A]

  import c.universe._

  def makeRoute[If: WeakTypeTag, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
      definition: c.Expr[Def]
  )(
      impl: c.Expr[Impl]
  ): c.Expr[Res] = {
    val ifP   = getPackage(weakTypeOf[If])
    val defT  = weakTypeOf[Def]
    val implT = weakTypeOf[Impl]

    val startInput = q"(HNil : HNil)"

    val res = for {
      dsl       <- constructDslTree(defT)
      wholeTree = new RouteTreeMaker(impl.tree).makeRouteTree(dsl, startInput)

      _ = infoTime(s"route $implT")

      tree = q"""
        {
          val $interface =  $ifP
          import shapeless.{::, HNil}

          $wholeTree
        }
        """

      checkedTree <- try {
                      c.typecheck(tree).asRight
                    } catch {
                      case typecheck: TypecheckException => typecheck.getMessage.asLeft
                    }
    } yield tree

    c.Expr(getOrError(res))
  }

  def makeResult[In <: HList: WeakTypeTag, Out: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
      in: c.Expr[In]
  )(
      impl: c.Expr[Impl]
  )(
      key: c.Expr[String]
  ): c.Expr[Res] = {
    val implT = weakTypeOf[Impl].dealias
    val inT   = weakTypeOf[In].dealias
    val outT  = weakTypeOf[Out].dealias
    val keySR: Result[String] = key.tree match {
      case Literal(Constant(s: String)) => NameTransformer.encode(s).asRight
      case _                            => "inproper use of `makeResult` key should be a string constant".asLeft
    }
    val methR = keySR.flatMap(key => findMeth(implT, TermName(key)))

    val rec  = extractRecord(inT)
    val syms = rec.flatten.map { case (paramName, _) => paramName -> freshName(paramName) }.toMap
    infoTime(s"result $outT $methR $implT")

    val paramsR = methR.flatMap(_.paramLists.traverse(_.traverse[Result, TermName] { p =>
      val name = symbolName(p)
      syms.get(name) toRight s"could not find input for parameter $name of method $keySR "
    }))

    val recpat = rec.foldRight(pq"_": Tree) {
      case (None, pat)            => pq"_ :: $pat"
      case (Some((name, _)), pat) => pq"(${syms(name)} @ _) :: $pat"
    }

    c.Expr(
      c.typecheck(
        getOrError(
          for {
            meth   <- methR
            params <- paramsR
          } yield q""" $in match { case $recpat =>
                        val res = $impl.$meth(...$params)
                        val route = $interface.route(res)
                        route[$inT, $outT]($in)}"""
        )))
  }

  val ConsC     = typeOf[:>[_, _]].typeConstructor
  val SplitC    = typeOf[<|>[_, _]].typeConstructor
  val dslAtom   = typeOf[DSLAtom]
  val CompleteC = typeOf[Complete[_]].typeConstructor
  val keyC      = typeOf[Key[_]].typeConstructor
  val interface = TermName("macroInterface")

  private class RouteTreeMaker(impl: Tree) {
    type DSL = DSLTree[Type]
    def makeRouteTree(dsl: DSL, input: Tree): Tree = dsl match {
      case DSLLeaf(resTyp, key) =>
        q"""{
          $interface.makeResult[$resTyp]($input)($impl)($key)
          }"""
      case DSLBranch(pref +: next, dsls) =>
        val identName = freshName("input")
        val tpt       = tq""
        val ident     = q"val $identName: $tpt"
        val rest      = makeRouteTree(DSLBranch(next, dsls), Ident(identName))
        q"""$interface.serve[$pref]($input).apply(($ident) => $rest)"""
      case DSLBranch(_, dsls) => makeRouteSumTree(dsls, input)
    }

    def makeRouteSumTree(dsls: Vector[DSL], input: Tree): Tree =
      dsls.map(makeRouteTree(_, input)).reduce { (l, r) =>
        q"""$interface.concatResults($l,$r)"""
      }
  }

  private def findMeth(typ: Type, name: Name): Result[MethodSymbol] =
    typ.decl(name) match {
      case ms: MethodSymbol                 => ms.asRight
      case ov if ov.alternatives.length > 1 => "could not handle method overloading".asLeft
      case _ =>
        NonEmptyList(s"method $name is not implemented" asLeft, typ.baseClasses.tail.collect {
          case base: TypeSymbol if base != typ => findMeth(base.toType, name)
        }).reduceK
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

  def constructDefPrefix(t: Type): Result[(Vector[Type], Option[String])] = t match {
    case Key(KeyName(key))  => (Vector(t), Some(key)).asRight
    case _ if t <:< dslAtom => (Vector(t), None).asRight
    case Cons(x, y) =>
      for {
        (a, k1) <- constructDefPrefix(x)
        (b, k2) <- constructDefPrefix(y)
      } yield (a ++ b, k1 orElse k2)
    case Split(t1, t2) => s"split over $t1 and $t2 in prefix: this is not allowed".asLeft
    case Complete(_)   => s"$t is usable only as tail element in the DSL".asLeft
  }

  def constructDslTree(t: Type, key: Option[String] = None, prefix: Vector[Type] = Vector.empty): Result[DSLTree[Type]] =
    t match {
      case Complete(res) =>
        key match {
          case Some(str) => DSLLeaf(res, str).asRight
          case None =>
            val typLine = (prefix :+ t).map(showType).mkString(" :> ")
            s"method key for $typLine is not defined".asLeft
        }

      case Cons(x, y) =>
        for {
          (p1, k1) <- constructDefPrefix(x)
          next     <- constructDslTree(y, key orElse k1, prefix = prefix ++ p1)
        } yield
          next match {
            case DSLBranch(p2, cdn) => DSLBranch(p1 ++ p2, cdn)
            case res                => DSLBranch(p1, Vector(res))
          }

      case Split(x, y) =>
        for {
          t1 <- constructDslTree(x, key, prefix)
          t2 <- constructDslTree(y, key, prefix)
        } yield
          t2 match {
            case DSLBranch(Vector(), cdn) => DSLBranch(Vector(), t1 +: cdn)
            case t2                       => DSLBranch(Vector(), Vector(t1, t2))
          }

      case _ if t <:< dslAtom => s"could not end DSL with $t".asLeft
      case _                  => s"type $t is not a part of type DSL".asLeft
    }

  def extractMethod(meth: MethodSymbol): MethodDecl[Type] =
    meth.paramLists.map(lst => lst.map(p => symbolName(p) -> p.typeSignature)) -> meth.returnType

  def symbolName(symbol: Symbol) = symbol.name.decodedName.toString

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
  private def infoTime(label: String) = if (true) info(s"$label: ${Instant.now().toString}")

  private def getOrError(res: Result[Tree]): Tree =
    res.fold({ err =>
      c.error(c.enclosingPosition, err)
      q"???"
    }, identity)
}

object MakerMacro {

  sealed trait DSLTree[T]
  case class DSLBranch[T](pref: Vector[T], children: Vector[DSLTree[T]]) extends DSLTree[T]
  case class DSLLeaf[T](res: T, key: String)                             extends DSLTree[T]

  type NList[T]      = List[(String, T)]
  type MethodDecl[T] = (List[NList[T]], T)
}
