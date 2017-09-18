import sbt._

object Resolvers {
  val tinkoff = Seq(
    "TCS Releases" at "http://nexus.tcsbank.ru/content/repositories/tcs",
    "TCS Snapshots" at "http://nexus.tcsbank.ru/content/repositories/tcs-snapshot",
    Resolver.bintrayRepo("hseeberger", "maven")
  )
}
