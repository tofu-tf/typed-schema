name := "Typed Schema"

moduleName := "typed-schema-all"

lazy val compilerPlugins = List(
  addCompilerPlugin("org.typelevel"  %% "kind-projector" % Version.kindProjector),
  addCompilerPlugin("org.scalamacros" % "paradise"        % "2.1.1" cross CrossVersion.patch)
)

lazy val commonSettings = compilerPlugins

lazy val kernel = project.in(file("modules/kernel")).settings(commonSettings)

lazy val macros = project.in(file("modules/macros")).dependsOn(kernel).settings(commonSettings)

lazy val swagger = project.in(file("modules/swagger")).dependsOn(kernel, macros).settings(commonSettings)

lazy val akkaHttp = project.in(file("modules/akkaHttp")).dependsOn(kernel, macros).settings(commonSettings)

lazy val main = project.in(file("modules/main")).dependsOn(kernel, macros, swagger, akkaHttp).settings(commonSettings)

lazy val scalaz = project.in(file("modules/scalaz")).dependsOn(main).settings(commonSettings)

lazy val typedschema =
  (project in file("."))
    .dependsOn(macros, kernel, main)
    .aggregate(macros, kernel, main, swagger, akkaHttp, scalaz)
