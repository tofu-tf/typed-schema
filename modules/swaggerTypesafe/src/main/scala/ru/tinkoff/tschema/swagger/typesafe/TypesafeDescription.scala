package ru.tinkoff.tschema.swagger.typesafe

import cats.syntax.option._
import com.typesafe.config.Config
import ru.tinkoff.tschema.swagger.PathDescription.{MethodTarget, Target, TypeTarget}
import ru.tinkoff.tschema.swagger.{PathDescription, SwaggerDescription}

object TypesafeDescription {
  def apply(cfg: Config): PathDescription.DescriptionMap = {
    def readKey(name: String): Option[SwaggerDescription] =
      if (cfg.hasPath(name)) cfg.getString(name).some else None

    {
      case Target.Tag(key)         => readKey(s"$key.tag")
      case Target.Method(key, sub) =>
        sub match {
          case MethodTarget.Path           => readKey(s"$key.path")
          case MethodTarget.Body           => readKey(s"$key.body")
          case MethodTarget.Summary        => readKey(s"$key.summary")
          case MethodTarget.Param(name)    => readKey(s"$key.params.$name")
          case MethodTarget.Status(status) => readKey(s"$key.status.$status")
        }
      case Target.Type(name, sub)  =>
        sub match {
          case TypeTarget.Type         => readKey(s"types.$name.info")
          case TypeTarget.Title        => readKey(s"types.$name.title")
          case TypeTarget.Field(field) => readKey(s"types.$name.values.$field")
        }
    }
  }
}
