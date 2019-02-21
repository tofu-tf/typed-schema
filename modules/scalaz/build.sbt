moduleName := "typed-schema-scalaz"

libraryDependencies ++=
  Vector(
    "org.scalaz" %% "scalaz-deriving" % Version.scalazDeriving,
    "org.scalaz" %% "deriving-macro"  % Version.scalazDeriving,
    compilerPlugin("org.scalaz" %% "deriving-plugin" % Version.scalazDeriving),
  )


def resourcesOnCompilerCp(config: Configuration): Setting[_] =
  managedClasspath in config := {
    val res = (resourceDirectory in config).value
    val old = (managedClasspath in config).value
    Attributed.blank(res) +: old
  }

resourcesOnCompilerCp(Compile)