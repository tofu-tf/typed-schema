organization in ThisBuild := "ru.tinkoff"
description in ThisBuild := "Typelevel DSL for defining webservices, covertible to akka-http and swagger definitions"
publishMavenStyle in ThisBuild := true

import com.typesafe.sbt.SbtGit.git

publishTo in ThisBuild := {
  val nexus = "http://nexus.tcsbank.ru/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/tcs-snapshot")
  else
    Some("releases" at nexus + "content/repositories/tcs")
}

val pubVersion = "0.8.2"

credentials in ThisBuild += Credentials(Path.userHome / ".ivy2" / ".credentials")

version in ThisBuild := {
  val branch = git.gitCurrentBranch.value
  if (branch == "master") pubVersion
  else s"$pubVersion-$branch-SNAPSHOT"
}

updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)


