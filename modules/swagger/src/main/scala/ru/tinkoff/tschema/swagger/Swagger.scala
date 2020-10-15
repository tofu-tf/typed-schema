package ru.tinkoff.tschema.swagger

import cats.{Endo, Eval}
import cats.syntax.option._
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait, TypeName}
import ru.tinkoff.tschema.swagger._
import ru.tinkoff.tschema.swagger.SwaggerTypeable.{Config, seq}

object Swagger extends Derivation[SwaggerTypeable] with SwaggerFunctions{
  type Typeclass[T] = SwaggerTypeable[T]

  def apply[A](implicit swagger: Swagger[A]): Swagger[A] = swagger

  private def calcTypeName(name: TypeName, cfg: Config, seen: Set[TypeName] = Set()): String =
    if (seen(name)) "#"
    else {
      val selfName: String = cfg.nameMod(if (cfg.useShortName) name.short else name.full)
      if (!cfg.mangleTypeParams || name.typeArguments.isEmpty) selfName
      else
        name.typeArguments.iterator
          .map(calcTypeName(_, cfg, seen + name))
          .mkString(
            selfName + "[",
            ", ",
            "]"
          )
    }

  def combine[T](caseClass: CaseClass[Typeclass, T])(implicit
      cfg: Config = SwaggerTypeable.defaultConfig,
      desc: DescribeTypeable[T] = DescribeTypeable.empty[T]
  ): Typeclass[T] =
    new Typeclass[T] {
      lazy val typ: SwaggerType =
        SwaggerRef(
          name = calcTypeName(caseClass.typeName, cfg),
          descr = desc.whole,
          typ = Eval.now(
            SwaggerObject(
              properties = caseClass.parameters.map { param =>
                SwaggerProperty(
                  name = cfg.propMod(param.label),
                  description = desc.element(param.label),
                  typ = Eval.later(param.typeclass.typ)
                )
              }.toVector,
              required = Eval.later(
                caseClass.parameters.toVector.collect {
                  case prop if !prop.typeclass.optional && prop.default.isEmpty => cfg.propMod(prop.label)
                }
              )
            )
          )
        )
    }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T])(implicit
      cfg: Config = SwaggerTypeable.defaultConfig,
      desc: DescribeTypeable[T] = DescribeTypeable.empty[T]
  ): Typeclass[T] =
    new Typeclass[T] {
      private val typeName = calcTypeName(sealedTrait.typeName, cfg)

      private val dtr = cfg.discriminator.map(cfg.propMod)

      private def addDiscriminator(caseName: String): Endo[SwaggerType] = {
        case SwaggerObject(properties, required, discriminator) =>
          SwaggerObject(
            properties ++ dtr.map(
              SwaggerProperty(
                _,
                None,
                Eval.later(SwaggerPrimitive.string.mod(_.copy(pattern = Some(cfg.altMod(caseName)))))
              )
            ),
            required.map(_ ++ dtr),
            discriminator
          )
        case SwaggerRef(_, descr, typ)                          =>
          SwaggerRef(s"$typeName.$caseName", descr, typ.map(addDiscriminator(caseName)))
        case other                                              => other
      }

      lazy val typ: SwaggerType =
        SwaggerRef(
          name = typeName,
          descr = desc.whole,
          typ = Eval.now(
            SwaggerOneOf(
              alts = sealedTrait.subtypes.map { sub =>
                cfg.altMod(sub.typeName.short).some -> Eval.later(
                  sub.typeclass.updateTyp(addDiscriminator(sub.typeName.short)).typ
                )
              }.toVector,
              discriminator = cfg.discriminator
            )
          )
        )
    }

  final implicit def swaggerListTypeable[T: SwaggerTypeable]: SwaggerTypeable[List[T]] = seq[List, T]

  implicit def derivedInstance[T]: Typeclass[T] = macro Magnolia.gen[T]
  def derive[T]: Typeclass[T] = macro Magnolia.gen[T]
  def instance[T]: Typeclass[T] = macro Magnolia.gen[T]
}
