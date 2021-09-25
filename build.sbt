import com.typesafe.sbt.SbtGit.git

val pubVersion = "0.15.0"

val publishSettings = List(
  name              := "Typed Schema",
  organization      := "ru.tinkoff",
  description       := "Typelevel DSL for defining webservices, convertible to akka-http/finagle and swagger definitions",
  publishMavenStyle := true,
  publishTo         := (
    if (isSnapshot.value)
      Some(Opts.resolver.sonatypeSnapshots)
    else
      sonatypePublishToBundle.value
  ),
  credentials ++= Option(Path.userHome / ".sbt" / ".ossrh-credentials")
    .filter(_.exists())
    .map(Credentials(_)),
  version           := {
    val branch = git.gitCurrentBranch.value
    if (branch == "master") pubVersion
    else s"$pubVersion-$branch-SNAPSHOT"
  },
  scmInfo           := Some(
    ScmInfo(
      url("https://github.com/TinkoffCreditSystems/typed-schema"),
      "git@github.com:username/projectname.git"
    )
  )
)

ThisBuild / licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / publishMavenStyle := true

ThisBuild / homepage   := Some(url("https://github.com/TinkoffCreditSystems/typed-schema"))
ThisBuild / developers := List(
  Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois"))
)

val minorVersion = SettingKey[Int]("minor scala version")

val scala212V = "2.12.14"
val scala213V = "2.13.6"

val crossCompile = crossScalaVersions := List(scala212V, scala213V)

val commonScalacOptions = scalacOptions ++= List(
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps"
)

val specificScalacOptions = scalacOptions ++= {
  minorVersion.value match {
    case 12 => List("-Ypartial-unification")
    case 13 => List("-Ymacro-annotations")
  }
}

val setMinorVersion      = minorVersion := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) => v.toInt
    case _            => 0
  }
}

lazy val compilerPlugins = libraryDependencies ++= List(
  compilerPlugin("org.typelevel" % "kind-projector"     % Version.kindProjector cross CrossVersion.patch),
  compilerPlugin("com.olegpy"   %% "better-monadic-for" % Version.bm4),
)

val paradise = libraryDependencies ++= {
  minorVersion.value match {
    case 12 => List(compilerPlugin("org.scalamacros" % "paradise" % Version.macroParadise cross CrossVersion.patch))
    case 13 => List()
  }
}

val magnolia = libraryDependencies += "com.propensive" %% "magnolia" % Version.magnolia

val tofuOptics =
  libraryDependencies ++= List("core", "macro").map(module => "tf.tofu" %% s"tofu-optics-$module" % Version.tofu)

val circe =
  libraryDependencies ++= List("core", "parser").map(module => "io.circe" %% s"circe-$module" % Version.circe) ++ List(
    "derivation",
    "derivation-annotations"
  ).map(module => "io.circe" %% s"circe-$module" % Version.circeDerivation)

val akkaHttpCirce = libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce

val catsCore   = "org.typelevel" %% "cats-core"   % Version.cats
val catsFree   = "org.typelevel" %% "cats-free"   % Version.cats
val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect
val shapeless  = "com.chuusai"   %% "shapeless"   % Version.shapeless
val enumeratum = "com.beachape"  %% "enumeratum"  % Version.enumeratum

val akkaHttpLib     = "com.typesafe.akka" %% "akka-http"         % Version.akkaHttp
val akkaTestKit     = "com.typesafe.akka" %% "akka-testkit"      % Version.akka     % Test
val akkaHttpTestKit = "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % Test
val finagleHttp     = "com.twitter"       %% "finagle-http"      % Version.finagle
val derevo          = "tf.tofu"           %% "derevo-cats"       % Version.derevo
val swaggerUILib    = "org.webjars.npm"    % "swagger-ui-dist"   % Version.swaggerUI
val scalaTags       = "com.lihaoyi"       %% "scalatags"         % Version.scalaTags
val env             = "tf.tofu"           %% "tofu-env"          % Version.tofu

val scalatest           = "org.scalatest"     %% "scalatest"       % Version.scalaTest           % Test
val scalacheck          = "org.scalacheck"    %% "scalacheck"      % Version.scalaCheck          % Test
val scalatestScalacheck = "org.scalatestplus" %% "scalacheck-1-15" % Version.scalaTestScalaCheck % Test

val akka   = List("actor", "stream").map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)
val zio    = List("dev.zio" %% "zio" % Version.zio, "dev.zio" %% "zio-interop-cats" % Version.zioCats)
val tethys = List("core", "jackson").map(module => "com.tethys-json" %% s"tethys-$module" % Version.tethys)

val reflect          = libraryDependencies += scalaOrganization.value   % "scala-reflect"           % scalaVersion.value
val compiler         = libraryDependencies += scalaOrganization.value   % "scala-compiler"          % scalaVersion.value
val collectionCompat = libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0"

val enumeratumCirce = "com.beachape" %% "enumeratum-circe" % Version.enumeratumCirce

val typesafeConfig = "com.typesafe" % "config" % Version.typesafeConfig

val swaggerUIVersion = SettingKey[String]("swaggerUIVersion")

lazy val testLibs = libraryDependencies ++= scalatest :: scalacheck :: scalatestScalacheck :: Nil

lazy val commonSettings       = publishSettings ++ List(
  scalaVersion := scala213V,
  collectionCompat,
  compilerPlugins,
  commonScalacOptions,
  specificScalacOptions,
  crossCompile,
  setMinorVersion,
  testLibs,
)

lazy val kernel               = project
  .in(file("modules/kernel"))
  .settings(
    commonSettings,
    moduleName := "typed-schema-typedsl",
    libraryDependencies ++= catsCore :: shapeless :: enumeratum :: Nil
  )

lazy val param                = project
  .in(file("modules/param"))
  .dependsOn(kernel)
  .settings(
    commonSettings,
    moduleName := "typed-schema-param",
    libraryDependencies += derevo,
    magnolia,
    paradise,
  )

lazy val macros               = project
  .in(file("modules/macros"))
  .dependsOn(kernel)
  .settings(
    commonSettings,
    moduleName := "typed-schema-macros",
    libraryDependencies ++= shapeless :: catsCore :: akkaHttpTestKit :: Nil,
    reflect
  )

lazy val swagger              = project
  .in(file("modules/swagger"))
  .dependsOn(kernel, macros)
  .settings(
    commonSettings,
    moduleName := "typed-schema-swagger",
    libraryDependencies ++= enumeratum :: derevo :: enumeratumCirce :: Nil,
    magnolia,
    tofuOptics,
    paradise,
    circe,
  )

lazy val akkaHttp             = project
  .in(file("modules/akkaHttp"))
  .dependsOn(kernel, macros, param)
  .settings(
    commonSettings,
    moduleName := "typed-schema-akka-http",
    libraryDependencies ++= akkaHttpLib :: akkaTestKit :: akkaHttpTestKit :: akka,
    akkaHttpCirce
  )

lazy val finagle              = project
  .in(file("modules/finagle"))
  .dependsOn(kernel, macros, param)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle",
    libraryDependencies ++= finagleHttp :: catsEffect :: catsFree :: Nil
  )

lazy val finagleCirce         = project
  .in(file("modules/finagleCirce"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-circe",
    circe
  )

lazy val finagleTethys        = project
  .in(file("modules/finagleTethys"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-tethys",
    libraryDependencies ++= tethys
  )

lazy val finagleCustom        = project
  .in(file("modules/finagleCustom"))
  .dependsOn(finagleCirce, finagleTethys, swagger)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-custom",
    libraryDependencies += derevo
  )

lazy val finagleZio           = project
  .in(file("modules/finagle-zio"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-zio",
    libraryDependencies ++= catsEffect :: zio
  )

lazy val finagleCommon        = project
  .in(file("modules/finagle-common"))
  .dependsOn(finagle, swagger)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-common"
  )

lazy val finagleEnv           = project
  .in(file("modules/finagle-env"))
  .dependsOn(finagle)
  .settings(
    commonSettings,
    moduleName := "typed-schema-finagle-env",
    libraryDependencies ++= catsEffect :: env :: Nil
  )

lazy val main                 = project
  .in(file("modules/main"))
  .dependsOn(kernel, macros, swagger, akkaHttp)
  .settings(
    commonSettings,
    moduleName := "typed-schema-base",
    libraryDependencies ++= akkaHttpLib :: akkaHttpTestKit :: akkaTestKit :: akka
  )

lazy val swaggerUI            =
  (project in file("modules/swaggerUI"))
    .dependsOn(swagger)
    .enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      moduleName       := "typed-schema-swagger-ui",
      libraryDependencies ++= swaggerUILib :: scalaTags :: Nil,
      swaggerUIVersion := {
        libraryDependencies.value
          .find(_.name == "swagger-ui-dist")
          .map(_.revision)
          .get
      },
      buildInfoKeys    := swaggerUIVersion :: Nil,
      buildInfoPackage := "ru.tinkoff.tschema.swagger"
    )

lazy val swaggerTypesafe      = project
  .in(file("modules/swaggerTypesafe"))
  .dependsOn(kernel, swagger)
  .settings(
    commonSettings,
    paradise,
    moduleName := "typed-schema-swagger-typesafe",
    libraryDependencies += typesafeConfig,
  )

lazy val swaggerTypesafeCheck = project
  .in(file("modules/swaggerTypesafeCheck"))
  .dependsOn(swaggerTypesafe)
  .settings(
    commonSettings,
    paradise,
    moduleName := "typed-schema-swagger-typesafe-check"
  )

lazy val typedschema          =
  (project in file("."))
    .dependsOn(macros, kernel, main)
    .settings(
      publish / skip := true,
      scalaVersion   := scala213V,
      publishSettings,
      setMinorVersion,
      crossCompile
    )
    .aggregate(
      macros,
      kernel,
      main,
      param,
      swagger,
      swaggerTypesafe,
      swaggerTypesafeCheck,
      akkaHttp,
      finagle,
      finagleZio,
      finagleEnv,
      finagleCirce,
      finagleTethys,
      finagleCommon,
      finagleCustom,
      swaggerUI,
    )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("checkfmt", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
