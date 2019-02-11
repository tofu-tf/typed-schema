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

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % Version.akka % Test
libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % Test

libraryDependencies += "org.scalatest" %% "scalatest" % Version.scalaTest % Test

libraryDependencies += "org.scalacheck" %% "scalacheck" % Version.scalaCheck % Test

libraryDependencies += "eu.timepit" %% "refined" % Version.refined

libraryDependencies ++= Seq("actor", "stream")
                        .map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)

libraryDependencies ++= Seq("core", "macro")
                        .map(module => "com.github.julien-truffaut" %% s"monocle-$module" % Version.monocle)

libraryDependencies ++= Seq("core")
                        .map(module => "org.typelevel" %% s"cats-$module" % Version.cats)

libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0"

libraryDependencies += "com.propensive" %% "magnolia" % Version.magnolia

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % Version.simulacrum

addCompilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)


addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)


libraryDependencies += { scalaOrganization.value } % "scala-compiler" % { scalaVersion.value }

sources in(Compile, doc) := Seq.empty


lazy val macros = project.dependsOn(typedsl)

lazy val typedsl = project

lazy val typedschema =
  (project in file("."))
    .dependsOn(macros)
    .aggregate(macros, typedsl)
