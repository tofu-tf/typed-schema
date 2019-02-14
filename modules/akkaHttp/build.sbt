moduleName := "typed-schema-akka-http"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % Version.akkaHttp

libraryDependencies ++= Seq("actor", "stream")
  .map(module => "com.typesafe.akka" %% s"akka-$module" % Version.akka)