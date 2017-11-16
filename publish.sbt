organization in ThisBuild := "ru.tinkoff"
description in ThisBuild := "Typelevel DSL for defining webservices, covertible to akka-http and swagger definitions"
publishMavenStyle in ThisBuild:= true
publishTo in ThisBuild := {
  val nexus = "http://nexus.tcsbank.ru/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/tcs-snapshot")
  else
    Some("releases" at nexus + "content/repositories/tcs")
}
credentials in ThisBuild += Credentials(Path.userHome / ".sbt" / ".credentials")
version in ThisBuild := "0.8.0"
updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)


