val swaggerUIVersion = SettingKey[Option[String]]("swaggerUIVersion")

val commonSettings = List(
  scalaVersion := "2.12.8",
  swaggerUIVersion := {
    libraryDependencies.value
      .find(_.name == "swagger-ui-dist")
      .map(_.revision)
  },
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    scalaVersion,
    sbtVersion,
    swaggerUIVersion
  ),
  buildInfoPackage := "ru.tinkoff.tschema.examples",
  libraryDependencies += "org.webjars.npm" % "swagger-ui-dist" % Version.swaggerUI,
  libraryDependencies += "com.lihaoyi" %% "scalatags" % Version.scalaTags,
  libraryDependencies += "org.typelevel" %% "cats-core" % Version.catsCore,
  libraryDependencies += "org.manatki" %% "derevo-tschema" % Version.derevo,
  libraryDependencies += "org.manatki" %% "derevo-circe" % Version.derevo,
  libraryDependencies += "ru.tinkoff" %% "typed-schema-swagger" % Version.typedSchema,
  addCompilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val akkaHttp = project
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-akka-http" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-scalaz" % Version.typedSchema,
    addCompilerPlugin(
      "org.scalaz" %% "deriving-plugin" % Version.scalazDeriving
    )
  )

lazy val finagle = project
  .enablePlugins(BuildInfoPlugin)
  .settings(
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-zio" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-tethys" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-scalaz" % Version.typedSchema
  )
