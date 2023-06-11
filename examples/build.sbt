val swaggerUIVersion = SettingKey[String]("swaggerUIVersion")

lazy val typedSchemaVersion = SettingKey[String]("typedSchemaVersion")

val scala212V = "2.12.15"
val scala213V = "2.13.11"

val commonSettings = List(
  scalaVersion                            := scala213V,
  typedSchemaVersion                      := {
    val branch = git.gitCurrentBranch.value
    if (branch == "master") Version.typedSchema
    else s"${Version.typedSchema}-$branch-SNAPSHOT"
  },
  crossScalaVersions                      := List(scala212V, scala213V),
  swaggerUIVersion                        := {
    libraryDependencies.value
      .find(_.name == "swagger-ui-dist")
      .map(_.revision)
      .get
  },
  buildInfoKeys                           := Seq[BuildInfoKey](
    name,
    version,
    scalaVersion,
    sbtVersion,
    swaggerUIVersion
  ),
  buildInfoPackage                        := "ru.tinkoff.tschema.examples",
  libraryDependencies += "org.webjars.npm" % "swagger-ui-dist" % Version.swaggerUI,
  libraryDependencies += "com.lihaoyi"    %% "scalatags"       % Version.scalaTags,
  libraryDependencies += "org.typelevel"  %% "cats-core"       % Version.catsCore,
  libraryDependencies += "org.manatki"    %% "derevo-cats"     % Version.derevo,
  libraryDependencies += "org.manatki"    %% "derevo-circe"    % Version.derevo,
  libraryDependencies += "org.manatki"    %% "derevo-tethys"   % Version.derevo,
  libraryDependencies += { "ru.tinkoff" %% "typed-schema-swagger" % typedSchemaVersion.value },
  libraryDependencies += { "ru.tinkoff" %% "typed-schema-swagger-ui" % typedSchemaVersion.value }
    .exclude("org.webjars.npm", "swagger-ui-dist"),
  addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.patch),
  addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
  scalacOptions ++= List(
    "language:higherKinds"
  ),
  libraryDependencies ++= {
    if (scalaVersion.value == scala212V)
      List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
    else Nil
  },
  scalacOptions ++= {
    if (scalaVersion.value == scala213V) List("-Ymacro-annotations")
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
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-akka-http" % typedSchemaVersion.value },
  )

lazy val finagleZio = project
  .dependsOn(common)
  .settings(commonSettings)
  .settings(
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-zio" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-tethys" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-circe" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-common" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-custom" % typedSchemaVersion.value },
  )

lazy val finagleEnv = project
  .dependsOn(common)
  .settings(commonSettings)
  .settings(
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-env" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-tethys" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-circe" % typedSchemaVersion.value },
    libraryDependencies += { "ru.tinkoff" %% "typed-schema-finagle-custom" % typedSchemaVersion.value },
    libraryDependencies += "org.typelevel" %% "simulacrum" % Version.simulacrum,
  )

lazy val example = project.in(file(".")).aggregate(common, akkaHttp, finagleZio, finagleEnv)
