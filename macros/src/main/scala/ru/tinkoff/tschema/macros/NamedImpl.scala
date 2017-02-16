package ru.tinkoff.tschema.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait NamedImpl[T, Input] {
  type Output
  def produce(input: Input, impl: T): Output
}

object NamedImpl {
  type Aux[T, I, O] = NamedImpl[T, I] {type Output = O}

  implicit def apply[T, Input](implicit routable: NamedImpl[T, Input]): Aux[T, Input, routable.Output] = routable

  implicit def materialize[T, Input, Output]: Aux[T, Input, Output] = macro NamedImplMacros.materialize[T, Input]
}

class NamedImplMacros(val c: whitebox.Context) extends shapeless.CaseClassMacros {
  import c.universe._
  import NamedImplMacros._
  import internal.{constantType, refinedType}

  def taggedType = typeOf[shapeless.tag.Tagged[_]].typeConstructor

  def materialize[T: WeakTypeTag, Input: WeakTypeTag]: Tree = try {
    val applier = new Applier(impl = weakTypeOf[T], input = weakTypeOf[Input])
    import applier._

    errors.foreach(err ⇒ info("ERROR during Named Implementation : " + err))
    if (!correct) abort("could not satisfy input type with implementation type")
    val outputStr = output.toString

    q"""new _root_.ru.tinkoff.tschema.macros.NamedImpl[$impl, $input]{
        import shapeless._
        type Output = $output
        def produce(union: $input, impl: $impl): Output = {
           val input = union.values
           $implementation
        }
      }"""
  } catch {
    case ex: Throwable ⇒
      info("ERROR during Named Implementation : " + ex)
      EmptyTree
  }

  def info[A](u: A): A = {
    c.info(c.enclosingPosition, u.toString, force = true)
    u
  }

  def extractMethods(tpe: Type): NList[(List[NList[Type]], Type)] =
    if (!tpe.typeSymbol.isClass || !tpe.typeSymbol.isAbstract) List()
    else tpe.decls.collect {
      case s: MethodSymbol ⇒ symbolName(s) ->
                             (s.paramLists.map(lst ⇒ lst.map(p ⇒ symbolName(p) → p.typeSignature)) -> s.returnType)
    }.toList

  def extractUnion(tpe: Type): NList[NList[Type]] =
    coproductElements(tpe).collect { case FieldType(KeyName(name), value) ⇒ name -> extractList(value) }

  def extractList(tpe: Type): NList[Type] =
    hlistElements(tpe).collect { case FieldType(KeyName(name), value) ⇒ name -> value }

  def symbolName(symbol: Symbol) = symbol.name.decodedName.toString

  case class ResultField(pattern: Tree, result: Tree, key: Type)

  case class TraverseState(correct: Boolean = true,
                           output: Vector[Type] = Vector.empty,
                           errors: Vector[String] = Vector.empty,
                           matches: Vector[ResultField] = Vector.empty) {
    def failed(msg: String) = copy(correct = false, errors = errors :+ msg)
    def addType(tpe: Type) = copy(output = output :+ tpe)
    def addMatch(mat: ResultField) = copy(matches = matches :+ mat)
  }

  class Applier(val input: Type, val impl: Type) {
    val inputWiden = input.dealias.widen
    val union = extractUnion(inputWiden)
    val methods = extractMethods(impl)

    lazy val TraverseState(correct, outputTypes, errors, matches) = {
      val implMap = methods.toMap
      union.foldLeft(TraverseState()) { case (state, (caseName, inputFields)) ⇒
        implMap.get(caseName) match {
          case None ⇒ state.failed(s"implementation for $caseName is not defined")
          case Some((caseImpl, retType)) ⇒
            traverseSingle(state, inputFields, caseImpl, retType, caseName)
        }
      }
    }

    def traverseSingle(init: TraverseState, input: NList[Type], impl: List[NList[Type]], retType: Type, name: String) = {
      val inputMap = input.toMap
      val keyType = KeyName(name)
      val retFld = FieldType(keyType, retType)

      val res = impl.flatten.foldLeft(init addType retFld) { case (state, (paramName, parImplType)) ⇒
        inputMap.get(paramName) match {
          case None ⇒ state.failed(s"parameter $paramName in method $name not found in input")
          case Some(parInputType) if parInputType <:< parImplType ⇒ state
          case Some(parInputType) ⇒
            state.failed(s"parameter $paramName in method $name has type $parImplType not assignable from $parInputType in input")
        }
      }

      def matc = {
        val nameMap = input.map { case (paramName, _) ⇒ paramName -> c.freshName(paramName) }.toMap
        val pattern = input
                      .map { case (pname, _) ⇒ TermName(nameMap(pname)) }
                      .foldRight[Tree](pq"HNil")((term, tail) ⇒ pq"$term :: $tail")

        val paramLists = impl.map(_ map { case (paramName, _) ⇒ Ident(TermName(nameMap(paramName))) })
        val funcName = TermName(name)
        val result = q"impl.$funcName(...$paramLists)"
        ResultField(pattern, result, keyType)
      }

      if (res.correct) res addMatch matc else res
    }

    lazy val output = mkCoproductTpe(outputTypes.toList)

    lazy val implementation = matches.foldRight[Tree](q"input") {
      case (ResultField(pat, res, key), next) ⇒ q"""input match{
          case Inl($pat) => Inl(labelled.field[$key]($res))
          case Inr(input) => Inr($next)
        } """
    }

    def infos = {
      info(s"implementation: $impl")
      info(s"input: $inputWiden")
      info(s"methods: $methods")
      info(s"union:  $union")
      info(s"output:  $output")
      info(s"matches: $matches")
    }
  }

  object KeyName {
    def apply(name: String): Type =
      NamedSymbol(appliedType(taggedType, constantType(Constant(name))))

    def unapply(tpe: Type): Option[String] = tpe match {
      case NamedSymbol(ConstantType(Constant(name: String))) ⇒ Some(name)
      case _ ⇒ None
    }
  }

  object NamedSymbol {
    def apply(tpe: Type) = refinedType(List(symbolTpe, tpe), NoSymbol)

    def unapply(tpe: Type) = tpe match {
      case RefinedType(List(sym, tag, _*), _) if sym == symbolTpe ⇒ tag.typeArgs.headOption
      case _ ⇒ None
    }
  }
}

object NamedImplMacros {
  type NList[T] = List[(String, T)]
  type =?>[-A, +B] = PartialFunction[A, B]
}

