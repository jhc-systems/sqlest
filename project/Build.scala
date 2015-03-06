import sbt._
import sbt.Keys._

import com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo
import com.typesafe.sbt.SbtGhPages
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtSite
import spray.boilerplate.BoilerplatePlugin._
import xerial.sbt.Sonatype._

object SqlestBuild extends Build {

  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(sqlest, extractors, examples),
    settings = commonSettings ++ Seq(
      moduleName := "root",

      publish := (),
      publishLocal := ()
    )
  )

  lazy val sqlest = Project(
    id = "sqlest",
    base = file("sqlest"),

    settings = commonSettings ++ publishingSettings ++ scaladocSettings ++ Boilerplate.settings ++ Seq(
      moduleName := "sqlest",

      libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test"
      )
    )
  ).dependsOn(extractors)

  lazy val extractors = Project(
    id = "extractors",
    base = file("extractors"),

    settings = commonSettings ++ publishingSettings ++ scaladocSettings ++ Boilerplate.settings ++ Seq(
      moduleName := "sqlest-extractors",

      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.6",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test"
      )
    )
  )

  lazy val examples = Project(
    id = "examples",
    base = file("examples"),

    settings = commonSettings ++ Seq(
      libraryDependencies += "com.h2database" % "h2" % "1.4.180",

      publish := (),
      publishLocal := ()
    )
  ).dependsOn(sqlest)

  def commonSettings = SbtScalariform.scalariformSettings ++ Seq(
    organization := "uk.co.jhc",
    version := "0.6.2-SNAPSHOT",
    scalaVersion := "2.11.5",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-language:implicitConversions", "-language:existentials")
  )

  def scaladocSettings = SbtSite.site.settings ++ SbtSite.site.includeScaladoc() ++ SbtGhPages.ghpages.settings ++ Seq(
    gitRemoteRepo := "git@github.com:jhc-systems/sqlest.git"
  )

  def publishingSettings = sonatypeSettings ++ Seq(
    // Publishing - http://www.scala-sbt.org/0.13/docs/Using-Sonatype.html
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials := Seq(Seq("SONATYPE_USER", "SONATYPE_PASSWORD").map(key => sys.env.get(key)) match {
      case Seq(Some(user), Some(password)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, password)
      case _                           =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }),
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
}
