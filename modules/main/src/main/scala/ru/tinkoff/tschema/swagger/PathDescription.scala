package ru.tinkoff.tschema.swagger

import java.util.{Locale, ResourceBundle}

import cats.syntax.option._
import ru.tinkoff.tschema.utils.UTF8ResourceBundle

object PathDescription {
  type DescriptionMap = Target => Option[SwaggerDescription]

  def i18n(bundle: ResourceBundle): DescriptionMap = {
    def readKey(name: String) = if (bundle.containsKey(name)) bundle.getString(name).some else None

    {
      case Target.Tag(key) => readKey(s"@$key")
      case Target.Method(key, sub) =>
        sub match {
          case MethodTarget.Path        => readKey(key)
          case MethodTarget.Body        => readKey(s"$key.body")
          case MethodTarget.Summary     => readKey(s"$key.summary")
          case MethodTarget.Param(name) => readKey(s"$key.$name")
        }
      case Target.Type(name, sub) =>
        sub match {
          case TypeTarget.Type         => readKey(s"~$name")
          case TypeTarget.Title        => readKey(s"~$name~")
          case TypeTarget.Field(field) => readKey(s"~$name~$field")
        }
    }
  }

  def utf8I18n(name: String, locale: Locale = Locale.getDefault) =
    i18n(UTF8ResourceBundle(name, locale))

  sealed trait Target

  object Target {
    final case class Method(name: String, sub: MethodTarget) extends Target
    final case class Tag(name: String)                       extends Target
    final case class Type(name: String, sub: TypeTarget)     extends Target

    implicit class DescriptionOps(val descriptionMap: DescriptionMap) extends AnyVal {
      def method(key: String): MethodTarget => Option[SwaggerDescription] =
        sub => descriptionMap(Method(key, sub))
      def typ(key: String): TypeTarget => Option[SwaggerDescription] =
        sub => descriptionMap(Type(key, sub))
    }
  }

  sealed trait MethodTarget
  object MethodTarget {
    case object Path                     extends MethodTarget
    case object Body                     extends MethodTarget
    case object Summary                  extends MethodTarget
    final case class Param(name: String) extends MethodTarget
  }

  sealed trait TypeTarget
  object TypeTarget {
    case object Type                     extends TypeTarget
    case object Title                    extends TypeTarget
    final case class Field(name: String) extends TypeTarget
  }
}
