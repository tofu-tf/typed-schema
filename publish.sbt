organization := "ru.tinkoff"

name := "typed-schema"

version := "0.3.3"

description := "Typelevel DSL for defining webservices, covertable to akka-http and swagger definitions"

publishMavenStyle := true

publishTo := {
  val nexus = "http://nexus.tcsbank.ru/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/tcs-snapshot")
  else
    Some("releases"  at nexus + "content/repositories/tcs")
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")