object Dependencies {
  object Main {
    object version {

    }

    val catsCore   = "org.typelevel" %% "cats-core"   % Version.cats
    val catsFree   = "org.typelevel" %% "cats-free"   % Version.cats
    val catsEffect = "org.typelevel" %% "cats-effect" % Version.catsEffect
    val enumeratum = "com.beachape"  %% "enumeratum"  % Version.enumeratum
    val enumeratumCirce = "com.beachape" %% "enumeratum-circe" % Version.enumeratumCirce
    val circe      =
      libraryDependencies ++= List("core", "parser").map(module => "io.circe" %% s"circe-$module" % Version.circe) ++ List(
        "derivation",
        "derivation-annotations"
      ).map(module => "io.circe" %% s"circe-$module" % Version.circeDerivation)
    val tethys = List("core", "jackson").map(module => "com.tethys-json" %% s"tethys-$module" % Version.tethys)
    val tofuOptics = List("core", "macro").map(module => "ru.tinkoff" %% s"tofu-optics-$module" % Version.tofu)

    val shapeless  = "com.chuusai"   %% "shapeless"   % Version.shapeless
    val magnolia   = "com.propensive" %% "magnolia"   % Version.magnolia

    val derevo        = "org.manatki"       %% "derevo-cats"       % Version.derevo
    val simulacrum    = "org.typelevel" %% "simulacrum"  % Version.simulacrum
    val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector"     % Version.kindProjector)
    val bm4           = compilerPlugin("com.olegpy"    %% "better-monadic-for" % Version.bm4)

    val paradise = compilerPlugin("org.scalamacros" % "paradise" % Version.macroParadise cross CrossVersion.patch)
    val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.4.1"

    val env = "ru.tinkoff"        %% "tofu-env"          % Version.tofu
    val zio = List("dev.zio" %% "zio" % Version.zio, "dev.zio" %% "zio-interop-cats" % Version.zioCats)

    val finagleHttp     = "com.twitter"       %% "finagle-http"      % Version.finagle

    val akka   = List("actor", "stream").map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)
    val akkaHttp     = "com.typesafe.akka" %% "akka-http"        % Version.akkaHttp
    val akkaHttpCirce = "de.heikoseeberger" %% "akka-http-circe" % Version.akkaHttpCirce
    val akkaTestKit     = "com.typesafe.akka" %% "akka-testkit"      % Version.akka     % Test
    val akkaHttpTestKit = "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp % Test

    val scalatest           = "org.scalatest"     %% "scalatest"       % Version.scalaTest           % Test
    val scalacheck          = "org.scalacheck"    %% "scalacheck"      % Version.scalaCheck          % Test
    val scalatestScalacheck = "org.scalatestplus" %% "scalacheck-1-15" % Version.scalaTestScalaCheck % Test
  }

  object Contrib {
    object version {

    }

    val scalaTags    = "com.lihaoyi"       %% "scalatags"         % Version.scalaTags
    val swaggerUILib = "org.webjars.npm"    % "swagger-ui-dist"   % Version.swaggerUI
  }
}