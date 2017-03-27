import sbt._
import sbt.Keys._

object Publish {
  lazy val settings = Seq(

    organization := "ru.tinkoff"
    ,
    description := "Typelevel DSL for defining webservices, covertable to akka-http and swagger definitions"
    ,
    publishMavenStyle := true
    ,
    publishTo := {
      val nexus = "http://nexus.tcsbank.ru/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/tcs-snapshot")
      else
        Some("releases" at nexus + "content/repositories/tcs")
    }
    ,
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
    ,
    version := "0.4.5"
  )
}