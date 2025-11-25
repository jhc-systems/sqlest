// Dependency resolution to fix version conflicts
ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")

addSbtPlugin("io.spray" % "sbt-boilerplate" % "0.6.1")

addSbtPlugin("com.github.sbt" % "sbt-site" % "1.6.0")

addSbtPlugin("com.github.sbt" % "sbt-ghpages" % "0.8.0")

addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.5.2")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.11")

addSbtPlugin("com.sonar-scala" % "sbt-sonar" % "2.3.0")