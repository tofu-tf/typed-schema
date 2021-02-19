package ru.tinkoff.tschema.http4s

import scala.reflect.macros.{ TypecheckException, blackbox }

class RoutesMacro(val c: blackbox.Context) {
  import c.universe._

  type WTTF[F[_]] = WeakTypeTag[F[Unit]]

  def convert[F[_]: WTTF, Def: WeakTypeTag, Impl: WeakTypeTag, Res: WeakTypeTag](
    definition: c.Expr[Def]
  )(
    impl: c.Expr[Impl]
  ): c.Expr[Res] = {
    val defT  = weakTypeOf[Def]
    val implT = weakTypeOf[Impl]
    val ft    = weakTypeOf[F[Unit]].typeConstructor

    val tree = q"""
       import cats.Defer
       import ru.tinkoff.tschema.http4s.Routing.Routes._
       implicit val deferF: Defer[$ft] = implicitly
       MkRoutes.apply.makeRoutes[$defT, $implT]($definition)($impl).toHttpRoutes
       """

    c.Expr(typeCheckOrAbort(tree))
  }

  def convertIn[F[_]: WTTF, Def: WeakTypeTag, Impl: WeakTypeTag, In: WeakTypeTag, Res: WeakTypeTag](
    definition: c.Expr[Def]
  )(
    impl: c.Expr[Impl]
  )(
    input: c.Expr[In]
  ): c.Expr[Res] = {
    val defT  = weakTypeOf[Def]
    val implT = weakTypeOf[Impl]
    val inT   = weakTypeOf[In]
    val ft    = weakTypeOf[F[Unit]].typeConstructor

    val tree = q"""
       import cats.Defer
       import ru.tinkoff.tschema.http4s.Routing.Routes._
       implicit val deferF: Defer[$ft] = implicitly
       MkRoutes.apply.makeRoutes[$defT, $implT, $inT]($definition)($impl)($input).toHttpRoutes
       """

    c.Expr(typeCheckOrAbort(tree))
  }

  private def typeCheckOrAbort(t: Tree): Tree =
    try c.typecheck(t)
    catch {
      case ex: TypecheckException => c.abort(c.enclosingPosition, ex.toString)
    }
}
