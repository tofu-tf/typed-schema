name := "typed-schema-macros"
resolvers ++= Resolvers.tinkoff
libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.chuusai" %% "shapeless" % Version.shapeless
libraryDependencies += "org.typelevel" %% "cats-core" % Version.cats
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % Version.catsMtl
libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akkaHttp
libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % "test"

addCompilerPlugin("org.spire-math" %% "kind-projector" % Version.kindProjector)