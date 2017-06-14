import sbt.Keys._
import sbt._

object Compiler {
  def settings: Seq[Setting[_]] = Seq(
    scalaVersion := "2.12.2-bin-typelevel-4"
    ,
    crossScalaVersions := Seq("2.11.11", "2.12.2-bin-typelevel-4")
    ,
    scalaOrganization := "org.typelevel"
    ,
    scalacOptions ++= Seq(
      "-Ypartial-unification",
      "-deprecation",
      "-feature",
      "-language:existentials",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps"
    )
    ,
    scalacOptions ++= {
      if (scalaVersion.value >= "2.12")
        Seq(
          "-Xstrict-patmat-analysis"
//      ,
//          "-Yinduction-heuristics"
        )
      else Seq()
    }
  )

}