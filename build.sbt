name := "typedschema"

Compiler.settings

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce

libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akka

libraryDependencies += "com.chuusai" %% "shapeless" % Version.shapeless

libraryDependencies ++= Seq("enumeratum", "enumeratum-circe")
                        .map(module => "com.beachape" %% module % Version.enumeratum)

libraryDependencies ++= Seq("core", "parser", "generic", "generic-extras")
                        .map(module => "io.circe" %% s"circe-$module" % Version.circe)


libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % Version.scalaTest % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % Version.scalaCheck % "test"

libraryDependencies += "eu.timepit" %% "refined" % Version.refined

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % Version.akka

addCompilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)

libraryDependencies += {scalaOrganization.value} % "scala-compiler" % {scalaVersion.value}


lazy val typedschema =
  (project in file("."))
  .dependsOn(macros)
  .aggregate(macros, typedsl)

lazy val macros =
  project
  .dependsOn(typedsl)
  .settings(
    name := "typed-schema-macros"
    ,
    Compiler.settings
    ,
    resolvers ++= Resolvers.tinkoff
    ,
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value
    ,
    libraryDependencies += "com.chuusai" %% "shapeless" % Version.shapeless
    ,
    libraryDependencies += "org.typelevel" %% "cats" % Version.cats
    ,
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akkaHttp
    ,
    libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % "test"
    ,
    libraryDependencies += "ru.tinkoff" %% "knapsack" % Version.knapsack
    ,
    addCompilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)
    ,
    Publish.settings
  )

lazy val typedsl =
  project
  .settings(
    name := "typed-schema-dsl"
    ,
    Compiler.settings
    ,
    Publish.settings
  )