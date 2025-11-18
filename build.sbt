import com.github.sbt.git.SbtGit.GitKeys.gitRemoteRepo
import com.jsuereth.sbtpgp.PgpKeys
import ReleaseTransformations._

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(sqlest, extractors, examples)

lazy val sqlest = (project in file("sqlest"))
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)
  .settings(moduleName := "sqlest")
  .settings(sqlestSettings: _*)
  .settings(
    mdocIn := file("docs") / "sqlest",
    mdocOut := file("."),
    libraryDependencies ++= Seq("com.typesafe.scala-logging" %% "scala-logging" % "3.9.6")
  ).dependsOn(extractors)

lazy val extractors = (project in file("extractors"))
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)
  .settings(moduleName := "sqlest-extractors")
  .settings(sqlestSettings: _*)
  .settings(
    mdocIn := file("docs") / "extractors",
    mdocOut := file("extractors"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "joda-time" % "joda-time" % "2.14.0",
      "org.joda" % "joda-convert" % "3.0.1"
    )
  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(libraryDependencies += "com.h2database" % "h2" % "2.4.240")
  .dependsOn(sqlest)

lazy val commonSettings = publishingSettings ++ Seq(
  organization := "uk.co.jhc",
  scalaVersion := "2.13.16",
  crossScalaVersions := List("2.13.16"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings"//,
//    "-Ystatistics:typer",
//    "-Xlog-implicit-conversions",
//    "-Xlog-implicits"
  ),
  coverageExcludedPackages := "sqlest.examples"
)

lazy val sqlestSettings = commonSettings ++ scaladocSettings ++ mdocSettings ++ Seq(
  mdocVariables := Map("VERSION" -> version.value),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    "com.chuusai" %% "shapeless" % "2.3.13" % "test",
    "com.h2database" % "h2" % "2.4.240" % "test"
  )
)

lazy val noPublishSettings = Seq(
  publish / skip := true,
  publishLocal / skip := true,
  publishArtifact := false
)

lazy val scaladocSettings = Seq(
  gitRemoteRepo := "git@github.com:jhc-systems/sqlest.git"
)

lazy val mdocSettings = Seq(
  mdocExtraArguments := Seq("--no-link-hygiene")
)

lazy val publishingSettings = sonatypeReleaseProcess ++ Seq(
  // Publishing - http://www.scala-sbt.org/0.13/docs/Using-Sonatype.html
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  publishTo := {
    val nexus = "https://nexus-proxy.lighthouse.jhc.uk/nexus/content/repositories/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "snapshots")
    else
      Some("releases" at nexus + "releases")
  },
  credentials += Credentials(
    "Sonatype Nexus Repository Manager",
    "nexus-proxy.lighthouse.jhc.uk",
    "dev", "jhcjhc"
  ),
  pomIncludeRepository := { _ => false },
  pomExtra := (
    <url>https://github.com/jhc-systems/sqlest</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com/jhc-systems/sqlest.git</url>
      <connection>scm:git:git@github.com/jhc-systems/sqlest.git</connection>
    </scm>
    <developers>
      <developer>
        <id>davegurnell</id>
        <name>Dave Gurnell</name>
      </developer>
      <developer>
        <id>brendanator</id>
        <name>Brendan Maginnis</name>
      </developer>
    </developers>)
)

lazy val sonatypeReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonaRelease", _), enableCrossBuild = true),
    pushChanges
  )
)
