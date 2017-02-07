name := "typedschema"

version := "1.0"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")


val akkaHttpVersion = "10.0.3"


libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % "1.12.0"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % akkaHttpVersion

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies ++= Seq("enumeratum", "enumeratum-circe")
  .map(module ⇒ "com.beachape" %% module % "1.5.2")

libraryDependencies ++= Seq("core", "parser", "generic", "generic-extras")
  .map(module ⇒ "io.circe" %% s"circe-$module" % "0.7.0")


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test"


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)