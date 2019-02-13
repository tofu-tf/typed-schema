moduleName := "typed-schema-typedsl"

libraryDependencies ++= Seq("core", "macro")
  .map(module => "com.github.julien-truffaut" %% s"monocle-$module" % Version.monocle)

libraryDependencies +=  "org.typelevel" %% s"cats-core" % Version.cats



libraryDependencies += "com.chuusai" %% "shapeless" % Version.shapeless

