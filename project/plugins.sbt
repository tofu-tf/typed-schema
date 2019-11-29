logLevel := Level.Warn

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC13")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")



addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.6")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")


// Docs
addSbtPlugin("com.eed3si9n"     % "sbt-unidoc"     % "0.4.2")
addSbtPlugin("com.47deg"        % "sbt-microsites" % "0.9.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages"    % "0.6.3")
addSbtPlugin("org.tpolecat"     % "tut-plugin"     % "0.6.11")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
