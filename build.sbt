name := "Typed Schema"

moduleName := "typed-schema-all"

lazy val compilerPlugins = List(
  addCompilerPlugin("org.typelevel"   %% "kind-projector"     % Version.kindProjector),
  addCompilerPlugin("org.scalamacros" % "paradise"            % "2.1.1" cross CrossVersion.patch),
  addCompilerPlugin("com.olegpy"      %% "better-monadic-for" % "0.3.0")
)

lazy val commonSettings = compilerPlugins

lazy val kernel = project.in(file("modules/kernel")).settings(commonSettings)

lazy val param = project.in(file("modules/param")).dependsOn(kernel).settings(commonSettings)

lazy val macros = project.in(file("modules/macros")).dependsOn(kernel).settings(commonSettings)

lazy val swagger = project.in(file("modules/swagger")).dependsOn(kernel, macros).settings(commonSettings)

lazy val akkaHttp = project.in(file("modules/akkaHttp")).dependsOn(kernel, macros, param).settings(commonSettings)

lazy val finagle    = project.in(file("modules/finagle")).dependsOn(kernel, macros, param).settings(commonSettings)
lazy val finagleZio = project.in(file("modules/finagle-zio")).dependsOn(finagle).settings(commonSettings)

lazy val main = project.in(file("modules/main")).dependsOn(kernel, macros, swagger, akkaHttp).settings(commonSettings)

lazy val scalaz = project.in(file("modules/scalaz")).dependsOn(main).settings(commonSettings)

lazy val typedschema =
  (project in file("."))
    .dependsOn(macros, kernel, main)
    .aggregate(macros, kernel, main, swagger, akkaHttp, scalaz, finagle, finagleZio)
