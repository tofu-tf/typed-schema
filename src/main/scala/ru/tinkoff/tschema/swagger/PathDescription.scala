package ru.tinkoff.tschema.swagger

import java.util.ResourceBundle
import cats.syntax.option._

final case class PathDescription(description: Option[SwaggerDescription], params: String => Option[SwaggerDescription])

object PathDescription {
  type DescriptionMap = String => Option[PathDescription]
  private def readBundle(bundle: ResourceBundle)(name: String): Option[SwaggerDescription] =
    if (bundle.containsKey(name)) bundle.getString(name).some else none


  def i18n(bundle: ResourceBundle): DescriptionMap = key =>
    PathDescription(
      readBundle(bundle)(key),
      param => readBundle(bundle)(s"$key.$param")
    ).some

  def partial(f: PartialFunction[(String, Option[String]), String]): DescriptionMap = key =>
    PathDescription(f.lift((key, none)), param => f.lift((key, param.some))).some
}
