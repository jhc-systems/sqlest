import sbt._
import sbt.Keys._

import com.typesafe.sbt.pgp.PgpKeys
import com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo
import com.typesafe.sbt.SbtGhPages
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.SbtSite
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
import scalariform.formatter.preferences._
import scoverage.ScoverageKeys._
import spray.boilerplate.BoilerplatePlugin._
import tut.Plugin._
import xerial.sbt.Sonatype._

object SqlestBuild extends Build {

  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(sqlest, extractors, examples),
    settings = commonSettings ++ noPublishSettings ++ Seq(
      moduleName := "root"
    )
  )

  lazy val sqlest = Project(
    id = "sqlest",
    base = file("sqlest"),

    settings = commonSettings ++ scaladocSettings ++ tutSettings ++ Boilerplate.settings ++ Seq(
      moduleName := "sqlest",

      tutSourceDirectory := file("docs") / "sqlest",
      tutTargetDirectory := file("."),
      tutNameFilter := """README.md""".r,

      libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test",
        "com.chuusai" %% "shapeless" % "2.1.0" % "test",
        "com.h2database" % "h2" % "1.4.180" % "test"
      )
    )
  ).dependsOn(extractors)

  lazy val extractors = Project(
    id = "extractors",
    base = file("extractors"),

    settings = commonSettings ++ scaladocSettings ++ tutSettings ++ Boilerplate.settings ++ Seq(
      moduleName := "sqlest-extractors",

      tutSourceDirectory := file("docs") / "extractors",
      tutTargetDirectory := file("extractors"),
      tutNameFilter := """README.md""".r,

      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.6",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test",
        "com.chuusai" %% "shapeless" % "2.1.0" % "test",
        "com.h2database" % "h2" % "1.4.180" % "test"
      )
    )
  )

  lazy val examples = Project(
    id = "examples",
    base = file("examples"),

    settings = commonSettings ++ noPublishSettings ++ Seq(
      libraryDependencies += "com.h2database" % "h2" % "1.4.180"
    )
  ).dependsOn(sqlest)

  def commonSettings = SbtScalariform.scalariformSettings ++ publishingSettings ++ Seq(
    organization := "uk.co.jhc",
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-unchecked",
      "-Xfatal-warnings",
      "-Yinline-warnings",
      "-Ywarn-dead-code",
      "-Xfuture"
    ),
    coverageExcludedPackages := "sqlest.examples",
    ScalariformKeys.preferences := ScalariformKeys.preferences.value
      .setPreference(DanglingCloseParenthesis, Preserve)
  )

  lazy val noPublishSettings = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  def scaladocSettings = SbtSite.site.settings ++ SbtSite.site.includeScaladoc() ++ SbtGhPages.ghpages.settings ++ Seq(
    gitRemoteRepo := "git@github.com:jhc-systems/sqlest.git"
  )

  def publishingSettings = sonatypeSettings ++ sonatypeReleaseProcess ++ Seq(
    // Publishing - http://www.scala-sbt.org/0.13/docs/Using-Sonatype.html
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials := {
      Seq("SONATYPE_USER", "SONATYPE_PASSWORD").map(sys.env.get) match {
        case Seq(Some(user), Some(password)) =>
          Seq(Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, password))
        case _ =>
          credentials.value
      }
    },
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
      ReleaseStep(action = Command.process("publishSigned", _)),
      setNextVersion,
      commitNextVersion,
      ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
      pushChanges
    )
  )
}
