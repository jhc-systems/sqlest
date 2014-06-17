import sbt._
import sbt.Keys._

import com.typesafe.sbt.SbtScalariform
import spray.boilerplate.BoilerplatePlugin.Boilerplate

object SqlestBuild extends Build {

  lazy val sqlest = Project(
    id = "sqlest",
    base = file("."),

    settings = SbtScalariform.scalariformSettings ++ Boilerplate.settings ++ Seq(
      organization := "jhc",
      name := "sqlest",
      version := "0.1.0-SNAPSHOT",

      scalaVersion := "2.11.1",
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),

      libraryDependencies ++= Seq(
        "joda-time" % "joda-time" % "2.2",
        "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
        "org.scalatest" %% "scalatest" % "2.1.7" % "test"
      ),

      // Publishing - http://www.scala-sbt.org/0.13/docs/Using-Sonatype.html
      publishMavenStyle := true,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
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
  )
}
