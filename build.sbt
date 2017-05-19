name := "typedschema"

Compiler.settings

val akkaHttpVersion = "10.0.5"
val akkaVersion = "2.5.1"

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % "1.12.0"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % akkaHttpVersion

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies ++= Seq("enumeratum", "enumeratum-circe")
                        .map(module ⇒ "com.beachape" %% module % "1.5.2")

libraryDependencies ++= Seq("core", "parser", "generic", "generic-extras")
                        .map(module ⇒ "io.circe" %% s"circe-$module" % "0.7.0")


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "eu.timepit" %% "refined" % "0.7.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaVersion

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += {scalaOrganization.value} % "scala-compiler" % {scalaVersion.value}

lazy val typedschema = (project in file("."))
                       .dependsOn(macros)
                       .aggregate(macros, typedsl)

lazy val macros = project.dependsOn(typedsl)

lazy val typedsl = project