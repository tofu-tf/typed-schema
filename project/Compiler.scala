import sbt.Keys._
import sbt._

object Compiler {
  def settings: Seq[Setting[_]] = Seq(
    scalaVersion := "2.12.1"
    ,
    crossScalaVersions := Seq("2.11.8", "2.12.1")
    ,
    scalaOrganization := "org.typelevel"
    ,
    scalacOptions += "-Yliteral-types"
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