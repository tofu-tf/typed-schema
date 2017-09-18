package ru.tinkoff.tschema.macros

import shapeless.Coproduct

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait NamedImpl[T, Input <: Coproduct] {
  type Output <: Coproduct
  def produce(input: Input, impl: T): Output
  def description: String = toString
}

object NamedImpl {
  type Aux[T, I <: Coproduct, O <: Coproduct] = NamedImpl[T, I] {type Output = O}

  implicit def apply[T, Input <: Coproduct](implicit impl: NamedImpl[T, Input]): Aux[T, Input, impl.Output] = impl

  implicit def materialize[T, Input]: NamedImpl[T, Input] = macro NamedImplMacros.materialize[T, Input]
}

class NamedImplMacros(val c: whitebox.Context) extends shapeless.CaseClassMacros with SymbolMacros{
  import c.universe._
  import NamedImplMacros._
  import internal.{constantType, refinedType}
  def materialize[T: WeakTypeTag, Input: WeakTypeTag]: Tree = try {
    val applier = new Applier(impl = weakTypeOf[T], input = weakTypeOf[Input])
    import applier._

    errors.foreach(err => info("ERROR during Named Implementation : " + err))
    if (!correct) abort("could not satisfy input type with implementation type")
    val outputStr = showNList(extractUnion(output))
    val inputStr = showAlg(union)
    val implStr = impl.toString

//    applier.infos

    q"""new _root_.ru.tinkoff.tschema.macros.NamedImpl[$impl, $input]{
        import shapeless._
        type Output = $output
        override def description =
          "NamedImpl[" + $implStr + "]\n" +
           "Input\n------------------\n" + $inputStr +
           "\nOutput\n--------------------\n" + $outputStr
        def produce(union: $input, impl: $impl): Output = {
           import shapeless.union._
           val input = union.values
           $implementation
        }
      }"""
  } catch {
    case ex: Throwable =>
      info("ERROR during Named Implementation : " + ex)
      EmptyTree
  }

  def info[A](u: A): A = {
    c.info(c.enclosingPosition, u.toString, force = true)
    u
  }

  def extractMethods(tpe: Type): NList[(List[NList[Type]], Type)] =
    tpe.decls.collect {
      case s: MethodSymbol => symbolName(s) ->
                             (s.paramLists.map(lst => lst.map(p => symbolName(p) -> p.typeSignature)) -> s.returnType)
    }.toList

  def extractAlgebra(tpe: Type): NList[NList[Type]] =
    extractUnion(tpe).map { case (name, product) => name -> extractList(product) }

  def extractUnion(tpe: Type): NList[Type] =
    coproductElements(tpe).collect { case FieldType(KeyName(name), value) => name -> value }

  def extractList(tpe: Type): NList[Type] =
    hlistElements(tpe).collect { case FieldType(KeyName(name), value) => name -> value }

  def showNList[A](lst: NList[A], prefix: String = "") =
    lst.iterator.map { case (name, value) => s"$name : $value" }
    .mkString(s"$prefix", "\n" + prefix, "")

  def showAlg[A](alg: NList[NList[A]]) =
    alg.map { case (name, value) => s"$name : \n${showNList(value, "    ")}" }.mkString("", "\n", "")

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
    val union = extractAlgebra(inputWiden)
    val methods = extractMethods(impl)

    lazy val TraverseState(correct, outputTypes, errors, matches) = {
      val implMap = methods.toMap
      union.foldLeft(TraverseState()) { case (state, (caseName, inputFields)) =>
        implMap.get(caseName) match {
          case None => state.failed(s"implementation for $caseName is not defined")
          case Some((caseImpl, retType)) =>
            traverseSingle(state, inputFields, caseImpl, retType, caseName)
        }
      }
    }

    def traverseSingle(init: TraverseState, input: NList[Type], impl: List[NList[Type]], retType: Type, name: String) = {
      val inputMap = input.toMap
      val keyType = KeyName(name)
      val retFld = FieldType(keyType, retType)

      val res = impl.flatten.foldLeft(init addType retFld) { case (state, (paramName, parImplType)) =>
        inputMap.get(paramName) match {
          case None => state.failed(s"parameter $paramName in method $name not found in input")
          case Some(parInputType) if parInputType <:< parImplType => state
          case Some(parInputType) =>
            state.failed(s"parameter $paramName in method $name has type $parImplType not assignable from $parInputType in input")
        }
      }

      def matc = {
        val nameMap = input.map { case (paramName, _) => paramName -> c.freshName(paramName) }.toMap
        val pattern = input
                      .map { case (pname, _) => TermName(nameMap(pname)) }
                      .foldRight[Tree](pq"HNil")((term, tail) => pq"$term :: $tail")

        val paramLists = impl.map(_ map { case (paramName, _) => Ident(TermName(nameMap(paramName))) })
        val funcName = TermName(name)
        val result = q"impl.$funcName(...$paramLists)"
        ResultField(pattern, result, keyType)
      }

      if (res.correct) res addMatch matc else res
    }

    lazy val output = mkCoproductTpe(outputTypes.toList)

    lazy val implementation = matches.foldRight[Tree](q"input") {
      case (ResultField(pat, res, key), next) => q"""input match{
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


}

object NamedImplMacros {
  type NList[T] = List[(String, T)]
  type =?>[-A, +B] = PartialFunction[A, B]
}

