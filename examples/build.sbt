val swaggerUIVersion = SettingKey[String]("swaggerUIVersion")

val commonSettings = List(
  scalaVersion := "2.12.10",
  crossScalaVersions := List("2.12.10", "2.13.1"),
  swaggerUIVersion := {
    libraryDependencies.value
      .find(_.name == "swagger-ui-dist")
      .map(_.revision)
      .get
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
  libraryDependencies += "org.manatki" %% "derevo-cats" % Version.derevo,
  libraryDependencies += "org.manatki" %% "derevo-circe" % Version.derevo,
  libraryDependencies += "org.manatki" %% "derevo-tethys" % Version.derevo,
  libraryDependencies += "ru.tinkoff" %% "typed-schema-swagger" % Version.typedSchema,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.patch),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  scalacOptions ++= List(
    "language:higherKinds"
  ),
  libraryDependencies ++= {
    if (scalaVersion.value == "2.12.10")
      List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
    else Nil
  },
  scalacOptions ++= {
    if (scalaVersion.value == "2.13.1") List("-Ymacro-annotations")
    else Nil
  }
)



lazy val common =
  project.settings(commonSettings).enablePlugins(BuildInfoPlugin)

lazy val akkaHttp = project
  .dependsOn(common)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-akka-http" % Version.typedSchema,
  )

lazy val finagleZio = project
  .dependsOn(common)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-zio" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-tethys" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-circe" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-common" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-custom" % Version.typedSchema,
  )

lazy val finagleEnv = project
  .dependsOn(common)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-env" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-tethys" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-circe" % Version.typedSchema,
    libraryDependencies += "ru.tinkoff" %% "typed-schema-finagle-custom" % Version.typedSchema,
    libraryDependencies += "org.typelevel" %% "simulacrum" % Version.simulacrum,
  )

lazy val example = project.in(file(".")).aggregate(common, akkaHttp, finagleZio, finagleEnv)
