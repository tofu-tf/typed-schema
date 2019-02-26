moduleName := "typed-schema-typedsl"

libraryDependencies ++= Seq("core", "macro")
  .map(module => "com.github.julien-truffaut" %% s"monocle-$module" % Version.monocle)

libraryDependencies +=  "org.typelevel" %% s"cats-core" % Version.cats

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % Version.simulacrum

libraryDependencies += "com.chuusai" %% "shapeless" % Version.shapeless

libraryDependencies += "com.beachape" %% "enumeratum"       % Version.enumeratum

libraryDependencies += "com.propensive" %% "magnolia" % Version.magnolia

libraryDependencies ++= Seq("core", "parser", "generic", "generic-extras")
  .map(module => "io.circe" %% s"circe-$module" % Version.circe)

