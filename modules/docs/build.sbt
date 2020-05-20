enablePlugins(MicrositesPlugin, ScalaUnidocPlugin, GhpagesPlugin)

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")
docsMappingsAPIDir := "api"

micrositeName := "Typed Schema"
micrositeDescription := "Typelevel DSL for defining webservices, convertible to akka-http and swagger definitions"
micrositeHighlightTheme := "atom-one-light"
micrositeHomepage := "http://tinkoffcreditsystems.github.io/typed-schema"
micrositeBaseUrl := "/typed-schema"
micrositeDocumentationUrl := "/typed-schema/api"
micrositeOrganizationHomepage := "https://www.tinkoff.ru/software/opensource"
micrositeTwitter := "@Tinkoff"
micrositeGithubOwner := "TinkoffCreditSystems"
micrositeGithubRepo := "typed-schema"
micrositePalette := Map(
  "brand-primary"   -> "#51839A",
  "brand-secondary" -> "#5A4C08",
  "brand-tertiary"  -> "#D1D4D8",
  "gray-dark"       -> "#192946",
  "gray"            -> "#424F67",
  "gray-light"      -> "#E3E2E3",
  "gray-lighter"    -> "#F4F3F4",
  "white-color"     -> "#FFFFFF"
)
micrositeAuthor := "Tinkoff"
micrositeFooterText := Some(
  """
    |<p>&copy; 2017-2019 <a href="https://github.com/TinkoffCreditSystems/typed-schema">TypedSchema Maintainers</a></p>
    |""".stripMargin
)
micrositeFavicons := Seq(
  microsites.MicrositeFavicon("favicon-16x16.png", "16x16"),
  microsites.MicrositeFavicon("favicon-32x32.png", "32x32")
)
autoAPIMappings := true
addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir)
ghpagesNoJekyll := false
organization := "TinkoffCreditSystems"
micrositePushSiteWith := GHPagesPlugin
fork in tut := true
fork in (ScalaUnidoc, unidoc) := true
scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
  "-Xfatal-warnings",
  "-doc-source-url",
  scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
  "-sourcepath",
  baseDirectory.in(LocalRootProject).value.getAbsolutePath,
  "-diagrams"
)
scalacOptions in Tut ~= (_.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-dead-code")))
git.remoteRepo := "git@github.com:TinkoffCreditSystems/typed-schema.git"
includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md"
