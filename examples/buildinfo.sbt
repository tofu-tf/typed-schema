val swaggerUIVersion = SettingKey[Option[String]]("swaggerUIVersion")

swaggerUIVersion := { libraryDependencies.value.find(_.name == "swagger-ui-dist").map(_.revision) }

buildInfoKeys := Seq[BuildInfoKey](
  swaggerUIVersion
)

enablePlugins(BuildInfoPlugin)

buildInfoPackage := "ru.tinkoff.tschema.examples"
