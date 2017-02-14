scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

scalaOrganization := "org.typelevel"

scalacOptions ++= Seq(
  "-Yinduction-heuristics",
  "-Yliteral-types",
  "-Xstrict-patmat-analysis"
)


libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
