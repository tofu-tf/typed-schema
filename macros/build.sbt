Compiler.settings

libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.0.5"

libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.0.5" % "test"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
