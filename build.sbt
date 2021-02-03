import com.typesafe.sbt.SbtGit.git

val pubVersion = "0.13.1"

val scala212V = "2.12.13"
val scala213V = "2.13.4"

val crossCompile = crossScalaVersions := List(scala212V, scala213V)

val publishSettings = List(
  name := "Typed Schema",
  organization := "ru.tinkoff",
  description := "Typelevel DSL for defining webservices, convertible to akka-http/finagle and swagger definitions",
  publishMavenStyle := true,
  publishTo := (
    if (isSnapshot.value)
      Some(Opts.resolver.sonatypeSnapshots)
    else
      sonatypePublishToBundle.value
  ),
  credentials ++= ((Path.userHome / ".sbt" / ".ossrh-credentials") :: Nil)
    .filter(_.exists())
    .map(Credentials(_)),
  version := {
    git.gitCurrentBranch.value match {
      case "master" => pubVersion
      case branch   => s"$pubVersion-$branch-SNAPSHOT"
    }
  },
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/TinkoffCreditSystems/typed-schema")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/TinkoffCreditSystems/typed-schema"),
      "git@github.com:TinkoffCreditSystems/typed-schema.git"
    )
  ),
  developers := List(
    Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois"))
  )
)

val minorVersion = SettingKey[Int]("minor scala version")

val commonScalacOptions = scalacOptions ++= List(
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps"
)

val setMinorVersion = minorVersion := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) => v.toInt
    case _            => 0
  }
}

lazy val commonSettings = publishSettings ++ List(
  scalaVersion := scala213V,
  libraryDependencies ++= List(
    kindProjector,
    bm4,
    collectionCompat,

    scalatest,
    scalacheck,
    scalatestScalacheck
  ),
  commonScalacOptions,
  scalacOptions ++= {
    minorVersion.value match {
      case 12 => List("-Ypartial-unification")
      case 13 => List("-Ymacro-annotations")
    }
  },
  crossCompile,
  setMinorVersion,
)

lazy val macroParadiseSettings = Seq(
  libraryDependencies ++= {
    minorVersion.value match {
      case 12 => List(paradise)
      case 13 => Nil
    }
  }
)

lazy val simulacrumSettings = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    simulacrum              % Provided
  ),
  pomPostProcess := { node =>
    import scala.xml.transform.{RewriteRule, RuleTransformer}

    new RuleTransformer(new RewriteRule {
      override def transform(node: xml.Node): Seq[xml.Node] = node match {
        case e: xml.Elem
            if e.label == "dependency" &&
              e.child.exists(child => child.label == "groupId" && child.text == simulacrum.organization) &&
              e.child.exists(child => child.label == "artifactId" && child.text.startsWith(s"${simulacrum.name}_")) =>
          Nil
        case _ => Seq(node)
      }
    }).transform(node).head
  }
)

lazy val typeDsl = project
  .in(file("modules/typedsl"))
  .settings(
    commonSettings,
    simulacrumSettings,
    moduleName := "typed-schema-typedsl",
    libraryDependencies ++= catsCore :: shapeless :: enumeratum :: Nil
  )

lazy val param = project
  .in(file("modules/param"))
  .dependsOn(typeDsl)
  .settings(
    commonSettings,
    moduleName := "typed-schema-param",
    libraryDependencies ++= magnolia :: derevo :: Nil,
    macroParadiseSettings,
  )

lazy val macros = project
  .in(file("modules/macros"))
  .dependsOn(typeDsl)
  .settings(
    commonSettings,
    moduleName := "typed-schema-macros",
    libraryDependencies ++= shapeless :: catsCore :: akkaHttpTestKit :: Nil,
    libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value
  )

lazy val swagger = project
  .in(file("modules/swagger"))
  .dependsOn(typeDsl, macros)
  .settings(
    commonSettings,
    simulacrumSettings,
    moduleName := "typed-schema-swagger",
    libraryDependencies ++= enumeratum :: tofuOptics :: magnolia :: derevo :: enumeratumCirce :: Nil,
    macroParadiseSettings,
    circe
  )

lazy val akkaHttp = project
  .in(file("modules/akka-http"))
  .dependsOn(typeDsl, macros, param)
  .settings(
    commonSettings,
    moduleName := "typed-schema-akka-http",
    libraryDependencies ++= akkaHttpLib :: akkaHttpCirce :: :: akkaTestKit :: akkaHttpTestKit :: akka,
  )

lazy val finagle = project
  .in(file("modules/finagle/finagle-core"))
  .dependsOn(typeDsl, macros, param)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle",
    libraryDependencies ++= finagleHttp :: catsEffect :: catsFree :: Nil
  )

lazy val finagleCommon = project
  .in(file("modules/finagle/finagle-common"))
  .dependsOn(finagle, swagger)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-common"
  )

lazy val finagleCirce = project
  .in(file("modules/finagle/finagle-circe"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-circe",
    libraryDependencies ++= circe :: Nil
  )

lazy val finagleTethys = project
  .in(file("modules/finagle/finagle-tethys"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-tethys",
    libraryDependencies ++= tethys
  )

lazy val finagleCustom = project
  .in(file("modules/finagle/finagle-custom"))
  .dependsOn(finagleCirce, finagleTethys, swagger)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-custom",
    libraryDependencies += derevo
  )

lazy val finagleZio = project
  .in(file("modules/finagle/finagle-zio"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-zio",
    libraryDependencies ++= catsEffect :: zio
  )

lazy val finagleEnv = project
  .in(file("modules/finagle-env"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-env",
    libraryDependencies ++= catsEffect :: env :: Nil
  )

lazy val docs = project
  .in(file("modules/docs"))
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    scalaVersion := scala213V,
    publish / skip := true,
    crossCompile,
    setMinorVersion,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(typeDsl, macros, swagger, akkaHttp)
  )
  .dependsOn(typeDsl, macros, akkaHttp)

lazy val contribSwaggerUI =
  (project in file("contrib/swagger-ui"))
    .dependsOn(swagger)
    .enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      moduleName := "typed-schema-swagger-ui",
      libraryDependencies ++= scalaTags :: swaggerUILib :: Nil,
      buildInfoKeys := Seq(
        "swaggerUIVersion" -> swaggerUILib.revision
      ),
      buildInfoPackage := "ru.tinkoff.tschema.swagger"
    )

lazy val typedschema =
  (project in file("."))
    .dependsOn(macros, typeDsl)
    .settings(
      publish / skip := true,
      scalaVersion := scala213V,
      publishSettings,
      setMinorVersion,
      crossCompile
    )
    .aggregate(
      macros,
      typeDsl,
      param,
      swagger,
      akkaHttp,
      finagle,
      finagleZio,
      finagleEnv,
      finagleCirce,
      finagleTethys,
      finagleCommon,
      finagleCustom,
      contribSwaggerUI,
      docs
    )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("checkfmt", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
