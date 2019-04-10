moduleName := "typed-schema"

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akkaHttp


libraryDependencies += "com.typesafe.akka" %% "akka-testkit"      % Version.akka     % Test
libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % Test

libraryDependencies += "org.scalatest" %% "scalatest" % Version.scalaTest % Test

libraryDependencies += "org.scalacheck" %% "scalacheck" % Version.scalaCheck % Test

libraryDependencies ++= Seq("actor", "stream")
  .map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)

libraryDependencies += "com.propensive" %% "magnolia" % Version.magnolia


libraryDependencies += { scalaOrganization.value } % "scala-compiler" % { scalaVersion.value }

sources in (Compile, doc) := Seq.empty