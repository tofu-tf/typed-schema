moduleName := "typed-schema-swagger"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akkaHttp

libraryDependencies ++= Seq("actor", "stream")
  .map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)

libraryDependencies += "io.circe" %% "circe-derivation" % Version.circeDerivation

libraryDependencies += "com.beachape" %% "enumeratum"       % Version.enumeratum
libraryDependencies += "com.beachape" %% "enumeratum-circe" % Version.enumeratumCirce
