name := "Typed Schema"

moduleName := "typed-schema"

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akkaHttp

libraryDependencies += "com.chuusai" %% "shapeless" % Version.shapeless

libraryDependencies += "com.beachape" %% "enumeratum" % Version.enumeratum
libraryDependencies += "com.beachape" %% "enumeratum-circe" % Version.enumeratumCirce

libraryDependencies ++= Seq("core", "parser", "generic", "generic-extras")
                        .map(module => "io.circe" %% s"circe-$module" % Version.circe)

libraryDependencies += "io.circe" %% "circe-derivation" % Version.circeDerivation

libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % Version.scalaTest % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % Version.scalaCheck % "test"

libraryDependencies += "eu.timepit" %% "refined" % Version.refined

libraryDependencies ++= Seq("actor", "stream")
                        .map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)

libraryDependencies ++= Seq("core", "macro")
                        .map(module => "com.github.julien-truffaut" %% s"monocle-$module" % Version.monocle)

libraryDependencies ++= Seq("core")
                        .map(module => "org.typelevel" %% s"cats-$module" % Version.cats)

libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.patch)

//addCompilerPlugin("io.tryp" % "splain" % "0.2.7" cross CrossVersion.patch)

libraryDependencies += { scalaOrganization.value } % "scala-compiler" % { scalaVersion.value }

sources in(Compile, doc) := Seq.empty


lazy val typedschema =
  (project in file("."))
    .dependsOn(macros)
    .aggregate(macros, typedsl)

lazy val macros = project.dependsOn(typedsl)

lazy val typedsl = project

lazy val examples = project
  .dependsOn(typedschema)
  .aggregate(typedschema)
