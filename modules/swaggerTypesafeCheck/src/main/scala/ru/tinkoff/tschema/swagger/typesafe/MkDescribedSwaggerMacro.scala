package ru.tinkoff.tschema.swagger.typesafe

import cats.Monoid
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.monoid._
import cats.data.Chain
import com.typesafe.config.ConfigFactory
import ru.tinkoff.tschema.macros.{MakerMacro, Skip}
import ru.tinkoff.tschema.swagger.MkSwagger.macroInterface
import ru.tinkoff.tschema.swagger.PathDescription.{DescriptionMap, MethodTarget, Target}
import ru.tinkoff.tschema.swagger.SwaggerBuilder

import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox

class MkDescribedSwaggerMacro(override val c: blackbox.Context) extends MakerMacro(c) {
  import c.universe._

  import AtomsTypes._

  def impl[Def: WeakTypeTag](definition: c.Expr[Def]): c.Expr[SwaggerBuilder] = {
    val result = makeRouteHNilNoImpl[Skip, macroInterface.type, Def, SwaggerBuilder](definition)

    checkTree(weakTypeOf[Def])

    result
  }

  private def checkTree(defTpe: Type): Unit = {

    val description = Description.get(c)

    // TODO: more cases - body params, statuses, result types
    def collectErrors(t: Type, methodName: MethodName): Chain[String] =
      t.dealias match {
        case Cons(x, y)                                =>
          val newMethodName = methodName |+| extractMethodName(x, methodName)
          collectErrors(x, newMethodName) ++ collectErrors(y, newMethodName)
        case Split(x, y)                               =>
          collectErrors(x, methodName) ++ collectErrors(y, methodName)
        case Key(KeyName(key))                         =>
          methodName.tag match {
            case Some(tag) =>
              val method = s"$tag.$key"
              checkExistence(Target.Method(method, MethodTarget.Summary))(s"There is no summary for $method")
            case None      =>
              Chain.one(s"There is no tag for method $key")
          }
        case Tag(KeyName(tag))                         =>
          checkExistence(Target.Tag(tag))(s"There is no description for tag $tag")
        // FIXME: add support of inner case classes, think about not trivial deriving
        case QueryParamAs(KeyName(name), _, rawReqTpe) =>
          val reqTpe = rawReqTpe.dealias
          methodName.value match {
            case Some(method) =>
              if (isCaseClass(reqTpe)) {
                val paramNames = caseClassParams(reqTpe)

                Chain.fromSeq(paramNames.toSeq).flatMap { paramName =>
                  checkExistence(Target.Method(method, MethodTarget.Param(paramName)))(
                    s"There is no description of param $paramName of method $method"
                  )
                }
              } else
                checkExistence(Target.Method(method, MethodTarget.Param(name)))(
                  s"There is no description of param $name of method $method"
                )
            case None         =>
              Chain.one(s"There is no key or tag for $reqTpe")
          }
        case _                                         => Chain.nil
      }

    def checkExistence(target: Target)(error: => String): Chain[String] =
      if (description(target).isEmpty)
        Chain.one(error)
      else Chain.nil

    val errors = collectErrors(defTpe, Monoid[MethodName].empty)

    if (errors.nonEmpty) {
      val message = errors.iterator.distinct.mkString("Errors while check Swagger Description: \n", "\n", "")
      if (c.settings.contains("warn-swagger-description"))
        c.abort(c.enclosingPosition, message)
      else
        c.info(c.enclosingPosition, message, force = false)
    }
  }

  private def extractMethodName(tpe: Type, cur: MethodName): MethodName =
    tpe.dealias match {
      case Tag(KeyName(tag)) => cur.copy(tag = tag.some)
      case Key(KeyName(key)) => cur.copy(key = key.some)
      case Cons(x, y)        =>
        extractMethodName(x, cur) |+| extractMethodName(y, cur)
      case _                 => cur
    }

  object AtomsTypes {
    import ru.tinkoff.tschema.typeDSL._

    object Tag          extends CombMatcher(typeOf[Tag[_]].typeConstructor)
    object QueryParamAs extends CombMatcher(typeOf[QueryParamAs[_, _, _]].typeConstructor)
  }

  case class MethodName(key: Option[String], tag: Option[String]) {
    def value: Option[String] = (key, tag).mapN((k, t) => s"$t.$k")
  }

  object MethodName {
    implicit val monoid: Monoid[MethodName] = new Monoid[MethodName] {
      val empty: MethodName = MethodName(None, None)

      def combine(x: MethodName, y: MethodName): MethodName = MethodName(x.key.orElse(y.key), x.tag.orElse(y.tag))
    }
  }

  private def isCaseClass(tpe: Type): Boolean = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass

  private def caseClassParams(tpe: Type): Iterable[String] =
    tpe.decls.collect {
      case m: MethodSymbol if m.isCaseAccessor =>
        m.asMethod.name.decodedName.toString
    }
}

object Description {
  private val cache: ThreadLocal[DescriptionMap] = new ThreadLocal()

  def get(c: blackbox.Context): DescriptionMap =
    cache.get() match {
      case null  =>
        val loaded = load(c)
        cache.set(loaded)
        loaded
      case value =>
        value
    }

  // TODO file name in properties
  private def load(c: blackbox.Context): DescriptionMap = {
    import java.nio.file.{Files, Paths}

    val paths = c.classPath.iterator.map(url => Paths.get(url.toURI)).filter(Files.isDirectory(_))
    val files = paths.map(_.toAbsolutePath.resolve("swagger.conf")).filter(Files.exists(_)).toList

    files match {
      case location :: Nil =>
        TypesafeDescription(ConfigFactory.parseString(new String(Files.readAllBytes(location))).resolve())
      case Nil             =>
        c.abort(c.enclosingPosition, "File swagger.conf is not found")
      case _               =>
        c.abort(c.enclosingPosition, "Multiple swagger.conf files are found")
    }
  }
}
