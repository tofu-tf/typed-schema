val swaggerUI = "3.9.1"

libraryDependencies += "org.webjars.npm" % "swagger-ui-dist" % swaggerUI
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.7"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.patch)
