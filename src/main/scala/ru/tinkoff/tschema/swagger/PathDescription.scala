package ru.tinkoff.tschema.swagger

import java.util.ResourceBundle
import cats.syntax.option._

object PathDescription {
  type PathDescription = Target => Option[SwaggerDescription]
  type DescriptionMap = String => PathDescription

  def i18n(bundle: ResourceBundle): DescriptionMap = {
    def readKey(name: String) = if (bundle.containsKey(name)) bundle.getString(name).some else None
    key => {
      case Target.Path        => readKey(key)
      case Target.Body        => readKey(s"$key.body")
      case Target.Param(name) => readKey(s"$key.$name")
    }
  }

  def partial(f: PartialFunction[(String, Target), String]): DescriptionMap =
    key => target => f.lift((key, target))

  sealed trait Target
  object Target {
    case object Path extends Target
    case object Body extends Target
    case class Param(name: String) extends Target
  }
}
}
