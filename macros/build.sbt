scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

scalaOrganization := "org.typelevel"

scalacOptions ++= {
  if (scalaVersion.value >= "2.12")
    Seq(
      "-Yinduction-heuristics",
      "-Yliteral-types",
      "-Xstrict-patmat-analysis"
    )
  else Seq()
}


libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
