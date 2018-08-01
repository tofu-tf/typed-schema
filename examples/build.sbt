val swaggerUI = "3.17.2"
val derevo = "0.4.5"

libraryDependencies += "org.webjars.npm" % "swagger-ui-dist" % swaggerUI
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.7"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "ru.tinkoff" %% "derevo-tschema" % derevo
libraryDependencies += "ru.tinkoff" %% "derevo-circe" % derevo

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)

//scalacOptions += "-Ymacro-debug-lite"