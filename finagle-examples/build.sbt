scalaVersion := "2.12.8"

val swaggerUIVersion = SettingKey[Option[String]]("swaggerUIVersion")

swaggerUIVersion := { libraryDependencies
  .value
  .find(_.name == "swagger-ui-dist").map(_.revision) }

lazy val examples = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion,
      sbtVersion, swaggerUIVersion),
    buildInfoPackage := "ru.tinkoff.tschema.examples"
  )

libraryDependencies += "org.webjars.npm" % "swagger-ui-dist" % Version.swaggerUI
libraryDependencies += "com.lihaoyi" %% "scalatags" % Version.scalaTags
libraryDependencies += "org.typelevel" %% "cats-core" % Version.catsCore

libraryDependencies += "org.manatki" %% "derevo-tschema" % Version.derevo
libraryDependencies += "org.manatki" %% "derevo-circe" % Version.derevo

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce
libraryDependencies += "ru.tinkoff" %% "typed-schema" % Version.typedSchema
libraryDependencies += "ru.tinkoff" %% "typed-schema-scalaz" % Version.typedSchema

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
addCompilerPlugin("org.scalaz" %% "deriving-plugin" % Version.scalazDeriving),
