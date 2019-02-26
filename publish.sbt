organization in ThisBuild := "ru.tinkoff"
description in ThisBuild := "Typelevel DSL for defining webservices, covertible to akka-http and swagger definitions"
publishMavenStyle in ThisBuild := true

import com.typesafe.sbt.SbtGit.git

publishTo in ThisBuild := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

val pubVersion = "0.11.0-beta1"

credentials in ThisBuild += Credentials(Path.userHome / ".sbt" / ".ossrh-credentials")

version in ThisBuild := {
  val branch = git.gitCurrentBranch.value
  if (branch == "master") pubVersion
  else s"$pubVersion-$branch-SNAPSHOT"
}

updateOptions in ThisBuild := updateOptions.value.withGigahorse(false)

scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/TinkoffCreditSystems/typed-schema"),
    "git@github.com:username/projectname.git"
  ))

licenses in ThisBuild += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
publishMavenStyle in ThisBuild := true


homepage in ThisBuild := Some(url("https://github.com/TinkoffCreditSystems/typed-schema"))
developers in ThisBuild := List(
  Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois")),
)
