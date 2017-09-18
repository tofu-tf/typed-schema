scalaVersion in ThisBuild := "2.12.3"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.3")
//scalaOrganization in ThisBuild := "org.typelevel"
scalacOptions in ThisBuild ++= Seq(
  "-Ypartial-unification",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps"
)
//scalacOptions in ThisBuild ++= {
//  if (scalaVersion.value >= "2.12")
//    Seq(
//      "-Xstrict-patmat-analysis"
//      //      ,
//      //          "-Yinduction-heuristics"
//    )
//  else Seq()
//}