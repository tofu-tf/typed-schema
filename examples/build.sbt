val swaggerUI = "3.17.2"
val derevo = "0.5.1"

libraryDependencies += "org.webjars.npm" % "swagger-ui-dist" % swaggerUI
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.7"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "org.manatki" %% "derevo-tschema" % derevo
libraryDependencies += "org.manatki" %% "derevo-circe" % derevo

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
